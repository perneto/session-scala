package uk.ac.ic.doc.sessionscala.compiler;

import org.scribble.protocol.model.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Created by: omp08
 */
public class UncheckedSession extends Session {

    public UncheckedSession(Session parent) {
        super(parent, Collections.<Activity>emptyList());
    }

    @Override
    public Session interaction(Role src, Role dst, MessageSignature msgSig) {
        return this;
    }

    @Override
    public Session dropMatchingRecursionLabel(Recursion r) {
        return this;
    }

    @Override
    public Session branchReceive(MessageSignature label, Role srcRole) {
        return this;
    }

    @Override
    protected Session branchSend(Role dst, Choice c, MessageSignature msgSig) {
        return this;
    }

    @Override
    public void checkBranchesSeen(List<MessageSignature> seen) {
        List<MessageSignature> copy = new ArrayList<MessageSignature>(seen);
        for (MessageSignature msig: seen) {
            copy.remove(msig);
            for (MessageSignature other: copy) {
                if (isMessageSignatureSubtype(msig, other)) {
                    throw new SessionTypeCheckingException
                            ("Branch label " + msig + " is a subtype of label " + other);
                }
            }
        }
    }

    @Override
    public Session dropFirst() {
        return this;
    }

    @Override
    public List<Activity> remaining() {
        return Collections.emptyList();
    }

    @Override
    public String toString() {
        return "UncheckedSession@" + System.identityHashCode(this);
    }

    @Override
    public LabelledBlock getRecur() {
        return null;
    }
}
