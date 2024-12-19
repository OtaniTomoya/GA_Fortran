module tree_generation
    use parameters
    use tree_structure
    implicit none
contains
    recursive subroutine create_random_tree(maxDepth, current_depth, node)
        integer, intent(in) :: maxDepth, current_depth
        type(TreeNode), pointer, intent(out) :: node
        real :: r

        allocate(node)
        call random_number(r)
        if (maxDepth <= current_depth .or. (current_depth >= MIN_DEPTH .and. r < 0.5)) then
            ! リーフノード
            node%is_leaf = .true.
            call random_number(r)
            node%prediction = int(r * NUM_CLASSES)
            !print *, "depth", current_depth
        else
            ! 内部ノード
            node%is_leaf = .false.
            call random_number(r)
            node%variable = int(r * NUM_FEATURES)
            call random_number(r)
            node%threshold = MIN_THRESHOLD + (MAX_THRESHOLD - MIN_THRESHOLD) * r
            call create_random_tree(maxDepth, current_depth + 1, node%left)
            call create_random_tree(maxDepth, current_depth + 1, node%right)
        end if
    end subroutine create_random_tree
end module tree_generation
