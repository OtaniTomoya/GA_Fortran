module leaf_label_update
    use parameters
    use tree_structure
    implicit none
contains
    subroutine update_leaf_labels(node, X, y, num_samples)
        type(TreeNode), pointer :: node
        real, intent(in) :: X(NUM_FEATURES, num_samples)
        integer, intent(in) :: y(num_samples)
        integer, intent(in) :: num_samples
        integer :: i

        ! リーフノードのラベルカウントを初期化
        call initialize_labels_counts(node)

        ! 各サンプルに対してツリーをトラバース
        do i = 1, num_samples
            call traverse_and_update(node, X(:, i), y(i))
        end do

        ! リーフノードの予測値を更新
        call update_leaf_predictions(node)
    end subroutine update_leaf_labels

    recursive subroutine initialize_labels_counts(node)
        type(TreeNode), pointer :: node
        if (.not. associated(node)) return
        if (node%is_leaf) then
            node%labels_counts = 0
        else
            call initialize_labels_counts(node%left)
            call initialize_labels_counts(node%right)
        end if
    end subroutine initialize_labels_counts

    recursive subroutine traverse_and_update(node, sample, label)
        type(TreeNode), pointer :: node
        real, intent(in) :: sample(:)
        integer, intent(in) :: label
        if (node%is_leaf) then
            node%labels_counts(label + 1) = node%labels_counts(label + 1) + 1
        else
            if (sample(node%variable + 1) <= node%threshold) then
                call traverse_and_update(node%left, sample, label)
            else
                call traverse_and_update(node%right, sample, label)
            end if
        end if
    end subroutine traverse_and_update

    recursive subroutine update_leaf_predictions(node)
        type(TreeNode), pointer :: node
        integer :: max_count, i
        if (.not. associated(node)) return
        if (node%is_leaf) then
            max_count = -1
            do i = 1, NUM_CLASSES
                if (node%labels_counts(i) > max_count) then
                    max_count = node%labels_counts(i)
                    node%prediction = i - 1
                end if
            end do
        else
            call update_leaf_predictions(node%left)
            call update_leaf_predictions(node%right)
        end if
    end subroutine update_leaf_predictions
end module leaf_label_update
