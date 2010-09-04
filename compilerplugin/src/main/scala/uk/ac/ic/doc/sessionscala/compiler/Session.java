package uk.ac.ic.doc.sessionscala.compiler;

import org.scribble.protocol.model.*;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

public class Session {
    private final HostTypeSystem hostTypeSystem;
    private final List<Activity> remaining;
    private final List<ImportList> imports;

    public Session(HostTypeSystem hostTypeSystem, ProtocolModel specified) {
        this(hostTypeSystem, specified.getImports(), listFromProtoModel(specified));
    }

    public Session(HostTypeSystem hostTypeSystem,
                   List<ImportList> imports, List<Activity> remaining) {
        this.hostTypeSystem = hostTypeSystem;
        this.remaining = new LinkedList<Activity>(remaining);
        // copies are important for immutability, see interaction and listFromProtoModel
        this.imports = imports;
        System.out.println("Created "+this+", remaining: "
                + this.remaining + ", imports: " + imports);
    }


    private static List<Activity> filterBehaviours(List<Activity> protocol) {
        List<Activity> copy = new LinkedList<Activity>(protocol);
        for (Iterator<Activity> it = copy.iterator(); it.hasNext(); ) {
            if (! (it.next() instanceof Behaviour)) {
                it.remove();
            }
        }
        return copy;
    }

    private static List<Activity> listFromProtoModel(ProtocolModel specified) {
        List<Activity> protocol = specified.getProtocol().getBlock().getContents();
        List<Activity> behaviours = filterBehaviours(protocol);
        System.out.println("listFromProtoModel:" + behaviours);
        return behaviours;
    }

    private static String whensToString(List<When> whens) {
        String s = "{";
        for (Iterator<When> it = whens.iterator(); it.hasNext(); ) {
            s += it.next().getMessageSignature();
            if (it.hasNext()) s += ", ";
        }
        return s + "}";
    }

    public Session interaction(Role src, Role dst, TypeReference msgType) {
        Activity expected = remaining.get(0);
        MessageSignature msgSig = new MessageSignature(msgType);

        if (expected instanceof Choice) {
            return branchSend(dst, expected, msgSig);
        } else {
            return interaction(src, dst, expected, msgSig);
        }
    }

    private Session interaction(Role src, Role dst, Activity expected, MessageSignature msgSig) {
        Interaction newInter = new Interaction(src, dst, msgSig);

        if (isSubtype(newInter, expected)) {
            remaining.remove(0);
            return new Session(hostTypeSystem, imports, remaining);
        } else {
            throw new SessionTypeCheckingException("Expected " + expected + " but got " + newInter);
        }
    }

    private Session branchSend(Role dst, Activity expected, MessageSignature msgSig) {
        Choice c = (Choice) expected;
        if (!c.getToRole().equals(dst))
            throw new SessionTypeCheckingException(
                    "Expected branch selection send to "
                            + c.getToRole() + " but got: " + dst);
        List<When> whens = c.getWhens();
        for (When when: whens) {
            if (isMessageSignatureSubtype(msgSig, when.getMessageSignature())) {
                remaining.remove(0);
                List<Activity> newRemaining = new LinkedList<Activity>(
                        filterBehaviours(when.getBlock().getContents())
                );
                newRemaining.addAll(remaining);
                return new Session(hostTypeSystem, imports, newRemaining);
            }
        }
        throw new SessionTypeCheckingException("Expected a branch label subtype among: "
                + whensToString(whens) + "but got: " + msgSig);
    }

    public Session visitBranch(MessageSignature label, Role srcRole) {
        Choice choice = getChoice();
        Role specSrcRole = choice.getFromRole();
        if (!specSrcRole.equals(srcRole))
            throw new SessionTypeCheckingException("Protocol had choice receive from " + specSrcRole
                    + ", but got: " + srcRole);

        List<When> whens = choice.getWhens();
        System.out.println(label);
        for (When w: whens) {
            System.out.println(w + ": " + w.getMessageSignature());
            if (isMessageSignatureSubtype(label, w.getMessageSignature()))
                return new Session(hostTypeSystem, imports, getRemaining(w));
        }
        throw new SessionTypeCheckingException("Accepting branch label " + label
                + ", but had no matching label. Available labels: " + whensToString(whens));
    }

    private Choice getChoice() {
        return (Choice) remaining.get(0);
    }

    public List<MessageSignature> missingBranches(List<MessageSignature> seen) {
        Choice c = getChoice();
        List<MessageSignature> missing = new LinkedList<MessageSignature>();
        for (When w: c.getWhens()) {
            MessageSignature whenSig = w.getMessageSignature();
            boolean found = false;
            for (MessageSignature seenSig: seen) {
                if (isMessageSignatureSubtype(seenSig, whenSig)) {
                    found = true; break;
                }
            }
            if (!found) missing.add(whenSig);
        }
        return missing;
    }

    public Session choiceChecked() {
        remaining.remove(0);
        return new Session(hostTypeSystem, imports, remaining);
    }

    private List<Activity> getRemaining(When w) {
        return filterBehaviours(w.getBlock().getContents());
    }

    public boolean isComplete() {
        return remaining().isEmpty();
    }

    public List<Activity> remaining() {
        //return Collections.unmodifiableList(remaining); copy because of Scalatest bug
        return new LinkedList<Activity>(remaining);
    }

    private boolean isSubtype(final Activity subtype, final Activity supertype) {
        class SubtypeVisitor extends AbstractModelObjectVisitor {
            boolean res = false;

            public void process(ModelObject obj) {
                // abstract, needs to be implemented, but no use here
            }

            @Override
            public void accept(Interaction supertype) {
                if (subtype instanceof Interaction) {
                    Interaction subInter = (Interaction) subtype;
                    boolean sendOk = supertype.getFromRole() == null
                            && supertype.getToRoles().equals(subInter.getToRoles());

                    boolean receiveOk = false;
                    if (supertype.getFromRole() != null)
                            receiveOk = supertype.getFromRole().equals(subInter.getFromRole());

                    MessageSignature superSig = supertype.getMessageSignature();
                    MessageSignature subSig = subInter.getMessageSignature();
                    boolean msgOk =  superSig != null
                            && subSig != null
                            && isMessageSignatureSubtype(subSig, superSig);
                    res = (sendOk ^ receiveOk) && msgOk;
                }
            }
        }
        SubtypeVisitor v = new SubtypeVisitor();
        supertype.visit(v);

        System.out.println("isSubtype, subtype: " + subtype + ", supertype: "
                + supertype + ", result: " + v.res);
        return v.res; // todo
    }

    private boolean isMessageSignatureSubtype(MessageSignature subSig, MessageSignature superSig) {
        boolean res;

        String superOp = superSig.getOperation();
        if (superOp != null) {
             res = superOp.equals(subSig.getOperation());
        } else {
             res = subSig.getOperation() == null;
        }

        Iterator<TypeReference> itSuper = superSig.getTypeReferences().iterator();
        Iterator<TypeReference> itSub = subSig.getTypeReferences().iterator();

        while (itSuper.hasNext() && itSub.hasNext()) {
            TypeReference trefSuper = itSuper.next();
            TypeReference trefSub = itSub.next();

            res &= hostTypeSystem.isSubtype(trefSub, trefSuper, imports);
        }

        res &= !itSuper.hasNext() && !itSub.hasNext();

        return res;
    }
}
