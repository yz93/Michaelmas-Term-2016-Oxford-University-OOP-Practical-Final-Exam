class Node(var key: Int, var value: Int, var left: Node, var right: Node)
class IntervalBST (private var root: Node) {
    def insert(key: Int, value: Int) {
        root = insert_helper(key, value, root)
    }
    // recursive implementation of inserting a key, value pair into a tree
    // returns the newly created node
    private def insert_helper(key: Int, value: Int, curr: Node): Node = {
        if (curr == null) new Node(key, value, null, null)
        else{
            if (intersect(key, value, curr.key, curr.value)) return null
            if (key < curr.key) curr.left = insert_helper(key, value, curr.left)
            else curr.right = insert_helper(key, value, curr.right)
            curr
        }
    }
    def delete(key: Int, value: Int) {
        delete_helper(key, value, root, null)
    }
    private def delete_helper(key: Int, value: Int, curr: Node, parent: Node) {
        if (curr == null) return
        else if (key == curr.key){
            // case 1: target node is a leaf
            if (curr.left == null && curr.right == null){
                // special case: target node is a leaf and is the root node
                if (parent == null) { 
                    root = null 
                }
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
                // normal case
                else if (parent.left.eq(curr)) parent.left = grandChild
                else parent.right = grandChild
            }
            // case 3: target node has two children
            else{
                var ptr: Node = curr.left
                var ptr_parent: Node = null
                // Find the largest-valued child in target node's left subtree
                while(ptr.right != null){
                    ptr_parent = ptr
                    ptr = ptr.right
                }
                val temp_k = ptr.key
                val temp_v = ptr.value
                // we've found the replacement node. Two cases: 1 child or leaf
                // case 1: leaf
                if (ptr.left == null && ptr.right == null){
                    if (ptr_parent.left.eq(ptr)) ptr_parent.left = null
                    else ptr_parent.right = null
                }
                // case 2: one child
                else {
                    var grandChild: Node = null
                    if (ptr.left != null) grandChild = ptr.left
                    else grandChild = ptr.right
                    if (ptr_parent.left.eq(ptr)) ptr_parent.left = grandChild
                    else ptr_parent.right = grandChild
                }
                curr.key = temp_k
                curr.value = temp_v
            }
        }
        else if (key < curr.key) delete_helper(key, value, curr.left, curr)
        else delete_helper(key, value, curr.right, curr)
    }
    
    // pos is the index where an insertion/deletion happened
    // insertion is true if an insertion happened, and false otherwise
    // update all relevant intervals in the tree to correct values
    // an pre-order traversal
    def update_intervals(pos: Int, isInsertion: Boolean){
        update_intervals_helper(pos, isInsertion, root)
    }
    
    private def update_intervals_helper(pos: Int, insertion: Boolean, curr: Node){
        if (curr == null) return
        else if (curr.key >= pos){
            if (insertion){
              curr.key += 1
              curr.value += 1
            }
            else{
                curr.key -= 1
                curr.value -= 1
            }
            update_intervals_helper(pos, insertion, curr.left)
            update_intervals_helper(pos, insertion, curr.right)
        }
        // skip left branch and go to right branch if pos > current node's key
        else {
            update_intervals_helper(pos, insertion, curr.right)
        }
    }

    // The first element in the tuple is not [-1, -1] if and only if key is contained 
    // by some interval in the tree, in which case, the first element represents that interval.
    // The second element in the tuple is true if and only if 
    // the interval [key, value] intersects with some interval in the tree
    def intersectAny(key: Int, value: Int, normal_pos: Boolean, for_insert_test: Boolean)
        : Tuple2[Tuple2[Int, Int], Boolean] = {
        intersectAny_helper(key, value, root, false, normal_pos, for_insert_test)
    }

    // recursive helper function for intersectAny
    private def intersectAny_helper(key: Int, value: Int, curr: Node, 
        any_intersect: Boolean, point: Boolean, for_insert_test: Boolean)
            : Tuple2[Tuple2[Int, Int], Boolean] = {
        if (curr == null) ((-1, -1), any_intersect)
        // the if condition checks for two scenarios
        // 1 point is before mark, so check key
        // 2 mark is before point, so check value
        else if ((point && key >= curr.key && key <= curr.value)
                 || (!point && value >= curr.key && value <= curr.value)){
            if (for_insert_test && key == curr.key) ((-1, -1), false) 
            else ((curr.key, curr.value), true)
        }
        else if (point){
            if (key < curr.key) {
                if (intersect(key, value, curr.key, curr.value))
                    intersectAny_helper(key, value, curr.left, true, point, for_insert_test)
                else intersectAny_helper(key, value, curr.left, false, point, for_insert_test)
            }
            else {
                if (intersect(key, value, curr.key, curr.value))
                    intersectAny_helper(key, value, curr.right, true, point, for_insert_test)
                else intersectAny_helper(key, value, curr.right, false, point, for_insert_test)
            }
        }
        else{
            if (value < curr.value) {
                if (intersect(key, value, curr.key, curr.value))
                    intersectAny_helper(key, value, curr.left, true, point, for_insert_test)
                else intersectAny_helper(key, value, curr.left, false, point, for_insert_test)
            }
            else {
                if (intersect(key, value, curr.key, curr.value))
                    intersectAny_helper(key, value, curr.right, true, point, for_insert_test)
                else intersectAny_helper(key, value, curr.right, false, point, for_insert_test)
            }
        }
          
    }
    // returns true if and only if interval (k1, v1) intersects with
    // interval (k2, v2)
    private def intersect(k1: Int, v1: Int, k2: Int, v2: Int): Boolean = {
        (k1 <= v2) && (v1 >= k2)
    }
    
}