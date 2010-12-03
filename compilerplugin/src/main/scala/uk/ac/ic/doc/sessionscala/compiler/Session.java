package uk.ac.ic.doc.sessionscala.compiler;

import org.scribble.protocol.model.*;

import java.util.Collections;
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
        this.remaining = Collections.unmodifiableList(remaining);
        //System.out.println("Created Session, remaining: " + remaining
        //        + ", remaining.id: " + System.identityHashCode(remaining));
        // copies are important for immutability, see interaction and listFromProtoModel
        this.imports = Collections.unmodifiableList(imports);
        //System.out.println("Created "+this+", remaining: " + this.remaining + ", imports: " + imports);
    }


    public Session(Session parent, List<Activity> remaining) {
        this(parent.hostTypeSystem, parent.imports, remaining);
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
        //System.out.println("listFromProtoModel:" + behaviours);
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
        checkNotEmpty("interaction");
        Activity expected = remaining.get(0);
        MessageSignature msgSig = new MessageSignature(msgType);

        if (expected instanceof Choice) {
            return branchSend(dst, (Choice) expected, msgSig);
        } else {
            return interaction(src, dst, expected, msgSig);
        }
    }

    private Session interaction(Role src, Role dst, Activity expected, MessageSignature msgSig) {
        Interaction newInter = new Interaction(src, dst, msgSig);

        if (isSubtype(newInter, expected)) {
            return dropFirst();
        } else {
            throw new SessionTypeCheckingException("Expected " + expected + " but got " + newInter);
        }
    }

    private void checkNotEmpty(String tried) {
        if (remaining.isEmpty())
            throw new SessionTypeCheckingException("Tried to do "+tried+", but protocol was finished");
    }
    public Session recursionLabel(Recursion r) {
        checkNotEmpty("recursive call");
        Activity a = remaining.get(0);
        if (! (a instanceof Recursion && ((Recursion) a).getLabel().equals(r.getLabel()))) 
          throw new SessionTypeCheckingException("Expected " + a
                  + ((a instanceof Recursion) ? " (label "+ ((Recursion) a).getLabel() + ")" : "")
                  +" but got " + r + " (label "+r.getLabel()+")");
        return dropFirst();
    }

    private <T> List<T> allButFirst(List<T> list) {
        List<T> newList = new LinkedList<T>();
        Iterator<T> it = list.iterator();
        if (it.hasNext()) it.next();
        while (it.hasNext()) newList.add(it.next());
        return newList;
    }

    private Session branchSend(Role dst, Choice c, MessageSignature msgSig) {
        if (!c.getToRole().equals(dst))
            throw new SessionTypeCheckingException(
                    "Expected branch selection send to "
                            + c.getToRole() + " but destination was: " + dst);
        List<When> whens = c.getWhens();
        for (When when: whens) {
            if (isMessageSignatureSubtype(msgSig, when.getMessageSignature())) {
                List<Activity> newRemaining = new LinkedList<Activity>(
                        filterBehaviours(when.getBlock().getContents())
                );
                newRemaining.addAll(allButFirst(remaining));
                return new Session(this, newRemaining);
            }
        }
        throw new SessionTypeCheckingException("Expected a branch label subtype among: "
                + whensToString(whens) + ", but got: " + msgSig);
    }

    public Session visitBranch(MessageSignature label, Role srcRole) {
        Choice choice = getChoice();
        Role specSrcRole = choice.getFromRole();
        if (!specSrcRole.equals(srcRole))
            throw new SessionTypeCheckingException("Protocol had choice receive from " + specSrcRole
                    + ", but got: " + srcRole);

        List<When> whens = choice.getWhens();
        for (When w: whens) {
            // The labels in user code can be supertypes of the protocol labels,
            // but there should be no ambiguity - a label can only cover one branch.
            // This is dealt with in missingBranches()
            if (isMessageSignatureSubtype(w.getMessageSignature(), label))
                return new Session(hostTypeSystem, imports, getRemaining(w));
        }
        throw new SessionTypeCheckingException("Accepting branch label " + label
                + ", but had no matching label. Available labels: " + whensToString(whens));
        // fixme: it's legal to have more branch receives than specified
    }

    public Choice getChoice() {
        assert remaining.size() > 0;
        Activity a = remaining.get(0);
        if (!(a instanceof Choice)) throw new SessionTypeCheckingException("Tried to make a Choice, but protocol had: " + a);
        return (Choice) a;
    }

    public List<MessageSignature> missingBranches(List<MessageSignature> seen) {
        Choice c = getChoice();
        List<MessageSignature> missing = new LinkedList<MessageSignature>();
        List<MessageSignature> seenCopy = new LinkedList<MessageSignature>(seen);
        for (When w: c.getWhens()) {
            MessageSignature whenSig = w.getMessageSignature();
            boolean found = false;
            for (Iterator<MessageSignature> it = seenCopy.iterator(); it.hasNext();) {
                MessageSignature seenSig = it.next();
                if (isMessageSignatureSubtype(whenSig, seenSig)) {
                    it.remove(); // This will ensure we return a non-empty list if
                    // a supertype label covers several branches, which is forbidden.
                    found = true; break;
                }
            }
            if (!found) missing.add(whenSig);
        }
        return missing;
    }

    public Session dropFirst() {
        return new Session(hostTypeSystem, imports, allButFirst(remaining));
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

    public static boolean isSubtype(final HostTypeSystem hostTypeSystem, final List<ImportList> imports, final Activity subtype, final Activity supertype) {
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
                            && isMessageSignatureSubtype(hostTypeSystem, imports, subSig, superSig);
                    res = (sendOk ^ receiveOk) && msgOk;
                }
            }
        }
        SubtypeVisitor v = new SubtypeVisitor();
        supertype.visit(v);

        //System.out.println("isSubtype, subtype: " + subtype + ", supertype: "
        //        + supertype + ", result: " + v.res);
        return v.res;
    }

    public static boolean isMessageSignatureSubtype(HostTypeSystem hostTypeSystem, List<ImportList> imports,  MessageSignature subSig, MessageSignature superSig) {
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

    private boolean isMessageSignatureSubtype(MessageSignature subSig, MessageSignature superSig) {
        return isMessageSignatureSubtype(hostTypeSystem, imports, subSig, superSig);
    }

    private boolean isSubtype(Activity subtype, Activity supertype) {
        return isSubtype(hostTypeSystem, imports, subtype, supertype);
    }

    private void throwIncompatible(Choice c) {
        throw new SessionTypeCheckingException("Expected: " + 
                (remaining.isEmpty() ? "<finished>" : remaining.get(0)) + " but got: " + c);
    }

    public Session findMatchingWhen(Role src, Role dst, When w) {
        if (src != null)        
            return visitBranch(w.getMessageSignature(), src);
        else
            return branchSend(dst, getChoice(), w.getMessageSignature());
    }

    @Override
    public String toString() {
        return "Session{" +
                "remaining=" + remaining +
                ", imports=" + imports +
                "}@" + System.identityHashCode(this);
    }

    public static boolean isSendChoice(Choice c) {
        return c.getFromRole() == null && c.getToRole() != null;
    }
    
    public LabelledBlock getRecur() {
        if (remaining.isEmpty()) return null;
        Activity act = remaining.get(0);
        if (act instanceof LabelledBlock) return (LabelledBlock) act;
        return null; 
    }
}
