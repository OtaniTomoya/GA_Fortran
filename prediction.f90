module prediction
    use tree_structure
    implicit none
contains
    integer function predict(node, sample)
        type(TreeNode), pointer :: node
        real, intent(in) :: sample(:)
        if (node%is_leaf) then
            predict = node%prediction
        else
            if (sample(node%variable + 1) <= node%threshold) then
                predict = predict(node%left, sample)
            else
                predict = predict(node%right, sample)
            end if
        end if
    end function predict
end module prediction
