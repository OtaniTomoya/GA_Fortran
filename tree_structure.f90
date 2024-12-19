module tree_structure
    use parameters
    implicit none

    type :: TreeNode
        integer :: variable = -1
        real   :: threshold = 0
        logical :: is_leaf = .false.
        integer :: prediction = -1
        integer :: labels_counts(NUM_CLASSES) = 0
        type(TreeNode), pointer :: left => null()
        type(TreeNode), pointer :: right => null()
    end type TreeNode

    type :: TreeNodePointer
        type(TreeNode), pointer :: ptr => null()
    end type TreeNodePointer

end module tree_structure
