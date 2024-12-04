module prediction
    use tree_structure
    implicit none
contains
    integer function predict(node, sample) result(res)
        type(TreeNode), pointer :: node
        integer, intent(in) :: sample(:)
        type(TreeNode), pointer :: current_node
        current_node => node

        do while (associated(current_node))
            if (current_node%is_leaf) then
                res = current_node%prediction
                return
            else
                if (sample(current_node%variable + 1) <= current_node%threshold) then
                    current_node => current_node%left
                else
                    current_node => current_node%right
                end if
            end if
        end do

        ! ノードがnullの場合のデフォルト値（必要に応じて調整）
        res = -1
    end function predict
end module prediction
