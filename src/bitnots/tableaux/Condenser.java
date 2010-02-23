package bitnots.tableaux;

import java.util.*;
import bitnots.util.PredicateBlock;

/**
 * This class is responsible for condensing a tableau once
 * a proof has been found, using a MultiBranchCloser.
 *
 * @author <a href="mailto:b-shults@bethel.edu">Benjamin Shults</a>
 * @version 1.0
 */

public class Condenser {

  // TODO consider what needs to happen when the user says, "I like this
  // Substitution".  All BCs that don't have compatible substitutions
  // need to be removed.  Also, the chosen substitution should be applied
  // everywhere.  (Also, all formulas everywhere need to have that substitution
  // added to their required?  Maybe not since the variables involved
  // should not really exist anymore.)
  
  // TODO But what about a BC?  If the user selects a BC (rather than
  // just its substitution) then the BC MUST be involved in every MBC.
  // This has consequences: 
  // 1. Everything done when a substitution was chosen can be done.
  // 2. All nodes below the chosen BC can be eliminated.
  // 3. All nodes that do not pertain to the BC between the BC and its
  //    nearest ancestor that is a child of one of BC's splits can be removed.
  // 4. BC's branch need never be expanded again.
  // 5. BC must be stored somewhere and its presence must be incorporated
  //    into the MultiBranchCloser search.
  
  // TODO here's part of what I can do in the above case:
  // 1. go to the BC's nearest ancestor N that is a child of one of BC's splits
  // 2. Create the very simple MBC that closes the tree rooted at N.
  // 3. Condense that MBC.
  // 4. Store that MBC at N.
  
  // TODO I want a function that takes a single BranchCloser or even just a
  // substitution and removes everything not compatible with it.
  
  // TODO: consider simply marking certain nodes as eliminated or "condensed"
  // and ignoring them until the proof is done?
  
}
