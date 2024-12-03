module tree_structure
    use parameters
    implicit none
    type :: TreeNode
        integer :: variable = -1
        real    :: threshold = 0.0
        logical :: is_leaf = .false.
        integer :: prediction = -1
        integer :: labels_counts(NUM_CLASSES) = 0
        type(TreeNode), pointer :: left => null()
        type(TreeNode), pointer :: right => null()
    end type TreeNode
contains
    ! ツリー関連のサブルーチンをここに記述します
end module tree_structure
