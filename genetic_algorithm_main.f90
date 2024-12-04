program genetic_algorithm_main
    use parameters
    use data_handling
    use tree_generation
    use evaluation
    use genetic_operators
    use tree_structure
    use prediction
    use,intrinsic :: iso_fortran_env
    implicit none

    ! 変数の宣言
    integer, allocatable :: X_train(:,:), X_test(:,:)
    integer, allocatable :: y_train(:), y_test(:)
    integer :: num_train, num_test
    type(TreeNodePointer), allocatable :: population(:), offspring(:), temp_population(:)
    real, allocatable :: fitness(:)
    integer :: i, generation
    real :: best_fitness, test_accuracy, mean_fitness
    type(TreeNodePointer) :: best_individual
    integer :: parent_indices(2)
    real :: r
    integer :: depth
    integer :: seed_size
    integer, allocatable :: seed(:)

    integer :: best_index

    integer :: time_begin_c,time_end_c, CountPerSec, CountMax

    call system_clock(time_begin_c, CountPerSec, CountMax)

    ! 乱数の初期化
    call random_seed(size=seed_size)
    allocate(seed(seed_size))
    seed = SEED_VALUE

    call random_seed(put=seed)

    ! データの読み込み
    call load_and_prepare_mnist(X_train, y_train, num_train, X_test, y_test, num_test)

    ! 初期集団の生成
    allocate(population(POPULATION_SIZE))
    allocate(fitness(POPULATION_SIZE))
    !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i, r, depth)
    do i = 1, POPULATION_SIZE
        call random_number(r)
        depth = int(MIN_DEPTH + (MAX_DEPTH - MIN_DEPTH) * r)
        call create_random_tree(depth, 0, population(i)%ptr)
    end do
    !$OMP END PARALLEL DO

    ! オフスプリングの配列を最初に割り当て
    allocate(offspring(POPULATION_SIZE))
    print '(A)', "Generation     max       mean"
    ! 世代ループ
    do generation = 1, GENERATIONS

        ! 適応度の計算
        !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i)
            do i = 1, POPULATION_SIZE
                fitness(i) = evaluate_individual(population(i)%ptr, X_train, y_train, num_train)
            end do
        !$OMP END PARALLEL DO
        ! 最良個体の選択
        if (generation == GENERATIONS) then
            best_index = maxloc(fitness, dim=1)
            call copy_tree(population(best_index)%ptr, best_individual%ptr)
        end if
        best_fitness = maxval(fitness)
        mean_fitness = SUM(fitness) / SIZE(fitness)
        print '(I10, F10.2, F10.2)', generation, best_fitness*100, mean_fitness*100

        ! オフスプリングの生成前に古いツリーを解放
        do i = 1, POPULATION_SIZE
            call deallocate_tree(offspring(i)%ptr)
        end do
        !$OMP END PARALLEL DO


        ! 選択と交叉
        do i = 1, POPULATION_SIZE, 2
            call roulette_wheel_selection(fitness, parent_indices)

            call copy_tree(population(parent_indices(1))%ptr, offspring(i)%ptr)
            call copy_tree(population(parent_indices(2))%ptr, offspring(i + 1)%ptr)

            call random_number(r)
            if (r < CROSSOVER_RATE) then
                call subtree_crossover(offspring(i)%ptr, offspring(i + 1)%ptr)
            end if
        end do

        ! 突然変異
        !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i)
        do i = num_elites + 1, POPULATION_SIZE
            call mutate(offspring(i)%ptr)
        end do
        !$OMP END PARALLEL DO

        ! 配列をスワップ
        call move_alloc(population, temp_population)
        call move_alloc(offspring, population)
        call move_alloc(temp_population, offspring)
    end do

    ! テストデータでの評価
    test_accuracy = evaluate_individual(best_individual%ptr, X_test, y_test, num_test)
    print *, "Test Accuracy of Best Individual: ", test_accuracy

    ! メモリの解放
    do i = 1, POPULATION_SIZE
        call deallocate_tree(population(i)%ptr)
        call deallocate_tree(offspring(i)%ptr)
    end do
    call deallocate_tree(best_individual%ptr)
    deallocate(population)
    deallocate(offspring)
    deallocate(fitness)
    deallocate(X_train)
    deallocate(y_train)
    deallocate(X_test)
    deallocate(y_test)
    deallocate(seed)

    call system_clock(time_end_c)

    print *,time_begin_c,time_end_c, CountPerSec,CountMax
    print *,real(time_end_c - time_begin_c)/CountPerSec,"sec"

contains

    subroutine roulette_wheel_selection(fitness, selected_indices)
        use parameters
        implicit none
        real, intent(in) :: fitness(:)
        integer, intent(out) :: selected_indices(2)
        integer :: i
        real :: total_fitness, cumulative_fitness(size(fitness))
        real :: r

        total_fitness = sum(fitness)
        cumulative_fitness(1) = fitness(1)
        do i = 2, size(fitness)
            cumulative_fitness(i) = cumulative_fitness(i - 1) + fitness(i)
        end do

        do i = 1, 2
            call random_number(r)
            r = r * total_fitness
            selected_indices(i) = 1
            do while (cumulative_fitness(selected_indices(i)) < r)
                selected_indices(i) = selected_indices(i) + 1
            end do
        end do
    end subroutine roulette_wheel_selection

    recursive subroutine deallocate_tree(node)
        type(TreeNode), pointer :: node
        if (associated(node)) then
            if (associated(node%left)) then
                call deallocate_tree(node%left)
            end if
            if (associated(node%right)) then
                call deallocate_tree(node%right)
            end if
            deallocate(node)
            nullify(node)
        end if
    end subroutine deallocate_tree

    recursive subroutine copy_tree(src, dest)
        type(TreeNode), pointer :: src
        type(TreeNode), pointer :: dest
        if (.not. associated(src)) then
            dest => null()
        else
            allocate(dest)
            dest%variable = src%variable
            dest%threshold = src%threshold
            dest%is_leaf = src%is_leaf
            dest%prediction = src%prediction
            dest%labels_counts = src%labels_counts
            if (associated(src%left)) then
                call copy_tree(src%left, dest%left)
            else
                dest%left => null()
            end if
            if (associated(src%right)) then
                call copy_tree(src%right, dest%right)
            else
                dest%right => null()
            end if
        end if
    end subroutine copy_tree

end program genetic_algorithm_main
