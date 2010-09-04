package uk.ac.ic.doc.sessionscala.compiler;

import org.scribble.protocol.model.ImportList;
import org.scribble.protocol.model.TypeReference;

import java.util.List;

/**
 * Created by: omp08
 */
public interface HostTypeSystem {
    boolean isSubtype(TypeReference subtype, TypeReference supertype, List<ImportList> imports);
}
