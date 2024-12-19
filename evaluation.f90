module evaluation
    use parameters
    use tree_structure
    use leaf_label_update
    use prediction
    implicit none
contains
    real function evaluate_individual(node, X, y, num_samples)
        type(TreeNode), pointer :: node
        integer, intent(in) :: X(NUM_FEATURES, num_samples)
        integer, intent(in) :: y(num_samples)
        integer, intent(in) :: num_samples
        integer :: correct, i
        integer :: pred

        ! リーフノードのラベル更新
        call update_leaf_labels(node, X, y, num_samples)

        ! 正解数のカウント
        correct = 0
        do i = 1, num_samples
            pred = predict(node, X(:, i))
            if (pred == y(i)) correct = correct + 1
        end do

        ! 精度の計算
        evaluate_individual = real(correct) / num_samples
    end function evaluate_individual

    real function pre(node, X, y, num_samples)
        type(TreeNode), pointer :: node
        integer, intent(in) :: X(NUM_FEATURES, num_samples)
        integer, intent(in) :: y(num_samples)
        integer, intent(in) :: num_samples
        integer :: correct, i
        integer :: pred

        ! 正解数のカウント
        correct = 0
        do i = 1, num_samples
            pred = predict(node, X(:, i))
            if (pred == y(i)) correct = correct + 1
        end do

        ! 精度の計算
        pre = real(correct) / num_samples
        
    end function pre
end module evaluation
