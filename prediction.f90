module prediction
    use tree_structure
    implicit none
contains
    recursive integer function predict(node, sample) result(res)
        type(TreeNode), pointer :: node
        real, intent(in) :: sample(:)
        if (node%is_leaf) then
            res = node%prediction
        else
            if (sample(node%variable + 1) <= node%threshold) then
                res = predict(node%left, sample)
            else
                res = predict(node%right, sample)
            end if
        end if
    end function predict
end module prediction
