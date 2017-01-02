class Node(var key: Int, var value: Int, var left: Node, var right: Node)
class IntervalBST (private var root: Node) {

    /** Inserts a (key, value) pair into the tree */
    def insert(key: Int, value: Int) {
        root = insert_helper(key, value, root)
    }

    /** recursive helper of insert(key, value) */
    private def insert_helper(key: Int, value: Int, curr: Node): Node = {
        // base case
        if (curr == null) new Node(key, value, null, null)
        else{
            // because of pre-screening this won't actually happen, nevertheless
            // a reasonable implementation if it were to happen
            if (intersect(key, value, curr.key, curr.value)) return null
            // branch to left
            if (key < curr.key) curr.left = insert_helper(key, value, curr.left)
            // branch to right
            else curr.right = insert_helper(key, value, curr.right)
            // return the updated subtree (the current root)
            curr
        }
    }

    /** Deletes a (key, value) pair from the tree*/
    def delete(key: Int, value: Int) {
        delete_helper(key, value, root, null)
    }

    /** recursive helper of delete(key, value) 
     *  Four cases: 1. No such node -> do nothing
     *              2. Target is a leaf node
     *              3. Target has a single child
     *              4. Target has two children
     */
    private def delete_helper(key: Int, value: Int, curr: Node, parent: Node) {
        // base case: no such node, just return
        if (curr == null) return
        // found target node, then deal with the three cases
        else if (key == curr.key){
            // case 1: target node is a leaf
            if (curr.left == null && curr.right == null){
                // special case: target node is a leaf and is the root node
                if (parent == null) root = null
                else if (parent.left eq curr) parent.left = null
                else parent.right = null
            }
            // case 2: target node has one child
            else if ((curr.left != null && curr.right == null) 
                      || (curr.left == null && curr.right != null)){
                var grandChild: Node = null
                if (curr.left != null) grandChild = curr.left
                else grandChild = curr.right
                // special case: target node is root
                if (parent == null) root = grandChild
                // normal case. Identity(reference) comparison is important here.
                else if (parent.left.eq(curr)) parent.left = grandChild
                else parent.right = grandChild
            }
            // case 3: target node has two children
            else{
                var ptr: Node = curr.left
                var ptr_parent: Node = curr
                // Find the largest-valued child in target node's left subtree
                while(ptr.right != null){
                    ptr_parent = ptr
                    ptr = ptr.right
                }
                val temp_k = ptr.key
                val temp_v = ptr.value
                // we've found the replacement node. Two cases: one child or leaf
                // case 1: leaf
                if (ptr.left == null && ptr.right == null){
                    if (ptr_parent.left.eq(ptr)) ptr_parent.left = null
                    else ptr_parent.right = null
                }
                // case 2: has one child
                else {
                    var grandChild: Node = null
                    if (ptr.left != null) grandChild = ptr.left
                    else grandChild = ptr.right
                    // Identity(reference) comparison is important here.
                    if (ptr_parent.left.eq(ptr)) ptr_parent.left = grandChild
                    else ptr_parent.right = grandChild
                }
                curr.key = temp_k
                curr.value = temp_v
            }
        }
        // branch to left to search for target node
        else if (key < curr.key) delete_helper(key, value, curr.left, curr)
        // branch to right to search for target node
        else delete_helper(key, value, curr.right, curr)
    }

    /** Because insertions and deletions can cause the positions of the encrypted
     *  blocks to change, ie, inserting a character on the left of the encrypted
     *  block causes the block to shift right by 1 and deleting a char before
     *  the block causes it to left shift by 1. 
     *  This function updates all affected intervals in the tree to their correct values.
     *  pos is the index where an insertion/deletion happened.
     *  difference is the supposed difference between the new position and 
     *  the old position of the intervals. Move the intervals by difference amount.
     *  Using pre-order traversal, the method prunes all intervals 
     *  that do not need to be updated.
     */
    def update_intervals(pos: Int, difference: Int){
        update_intervals_helper(pos, difference, root)
    }

    /** recursive helper of update_intervals */
    private def update_intervals_helper(pos: Int, difference: Int, curr: Node){
        // base case
        if (curr == null) return
        else if (curr.key >= pos){
            // update current node
            curr.key += difference
            curr.value += difference
            // update left branch
            update_intervals_helper(pos, difference, curr.left)
            // update right branch
            update_intervals_helper(pos, difference, curr.right)
        }
        // Pruning: skip left branch and go to right branch if pos > current node's key
        else {
            update_intervals_helper(pos, difference, curr.right)
        }
    }

    /** Returns a 2-tuple of type ((Int, Int), Boolean)
     *  If the cursor lies within an encrypted block, then the first
     *  element, (Int, Int), is the start and end position of that block, 
     *  otherwise the first element is (-1, -1).
     *  The second element in the tuple is true if and only if the interval 
     *  [key, value] intersects with some interval in the tree.
     *  If normal_pos is true, then key indicates the cursor position
     *  Otherwise, value indicates the cursor position.
     *  for_insert_test indicates whether this method is being used by
     *  Editor.insertCommand to check if it can insert at position key.
     *  for_insert_test is necessary because even if the insertion point
     *  is the same as the starting position of an encrypted block, which in
     *  normal case is regarded as an intersection, we still want to be able
     *  to insert, because such insertion does not modify the block.
     *  for_insert_test is only true when this method is invoked in 
     *  Editor.insertCommand, in which case key = value.
     */
    def intersectAny(key: Int, value: Int, normal_pos: Boolean, for_insert_test: Boolean)
        : Tuple2[Tuple2[Int, Int], Boolean] = {
        intersectAny_helper(key, value, root, false, normal_pos, for_insert_test)
    }

    /** recursive helper function for intersectAny */
    private def intersectAny_helper(key: Int, value: Int, curr: Node, 
        any_intersect: Boolean, normal_pos: Boolean, for_insert_test: Boolean)
            : Tuple2[Tuple2[Int, Int], Boolean] = {
        // base case
        if (curr == null) ((-1, -1), any_intersect)
        // the if condition checks for two scenarios
        // 1) point is before mark, so check key
        // 2) point is after mark, so check value
        else if ((normal_pos && key >= curr.key && key <= curr.value)
                 || (!normal_pos && value >= curr.key && value <= curr.value)){
            if (for_insert_test && key == curr.key) ((-1, -1), false)
            // cursor is in some encrypted block; return that interval for use of deciphering
            else ((curr.key, curr.value), true)
        }
        // normal position (defined previously and in report) -> compare key with curr.key
        else if (normal_pos){
            // use if_intersect to remember if [key, value] intersects with
            // some interval and pass down the value. Because we do not immediately
            // return if there is an intersection. We return only after
            // we made sure either the cursor is or is not in any encrypted block.
            if (key < curr.key)
                if (intersect(key, value, curr.key, curr.value))
                    intersectAny_helper(key, value, curr.left, true, normal_pos, for_insert_test)
                else
                    intersectAny_helper(key, value, curr.left, any_intersect, normal_pos, for_insert_test)
            else
                if (intersect(key, value, curr.key, curr.value))
                    intersectAny_helper(key, value, curr.right, true, normal_pos, for_insert_test)
                else
                    intersectAny_helper(key, value, curr.right, any_intersect, normal_pos, for_insert_test)
        }
        // unusual position -> compare value with curr.value
        else{
            if (value < curr.value)
                if (intersect(key, value, curr.key, curr.value))
                    intersectAny_helper(key, value, curr.left, true, normal_pos, for_insert_test)
                else
                    intersectAny_helper(key, value, curr.left, any_intersect, normal_pos, for_insert_test)
            else
                if (intersect(key, value, curr.key, curr.value))
                    intersectAny_helper(key, value, curr.right, true, normal_pos, for_insert_test)
                else
                    intersectAny_helper(key, value, curr.right, any_intersect, normal_pos, for_insert_test)
        }
    }

    /** Returns true if and only if interval [k1, v1] intersects with interval [k2, v2] */
    private def intersect(k1: Int, v1: Int, k2: Int, v2: Int): Boolean = {
        (k1 <= v2) && (v1 >= k2)
    }
}