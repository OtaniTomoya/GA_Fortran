program genetic_algorithm_main
    use parameters
    use data_handling
    use tree_generation
    use evaluation
    use genetic_operators
    use tree_structure  ! TreeNodePointer 型はこのモジュールから取得
    use prediction
    implicit none

    ! 変数の宣言
    real, allocatable :: X_train(:,:), X_test(:,:)
    integer, allocatable :: y_train(:), y_test(:)
    integer :: num_train, num_test
    type(TreeNodePointer), allocatable :: population(:)
    real, allocatable :: fitness(:)
    integer :: i, generation
    real :: best_fitness, test_accuracy
    type(TreeNode), pointer :: best_individual
    type(TreeNodePointer), allocatable :: offspring(:)
    integer :: parent_indices(2)
    real :: r
    integer :: depth
    integer :: seed_size
    integer, allocatable :: seed(:)
    real, parameter :: EPSILON = 1.0e-6
    type(TreeNodePointer), allocatable :: elite_population(:)
    integer :: num_elites

    ! 乱数の初期化
    call random_seed(size=seed_size)
    allocate(seed(seed_size))
    seed = [(SEED_VALUE, i = 1, seed_size)]  ! シードを seed_size 分だけ埋める
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

    ! 世代ループ
    do generation = 1, GENERATIONS
        ! 適応度の計算
        !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i)
        do i = 1, POPULATION_SIZE
            fitness(i) = evaluate_individual(population(i)%ptr, X_train, y_train, num_train)
        end do
        !$OMP END PARALLEL DO

        ! エリート個体を選択
        num_elites = 1
        allocate(elite_population(num_elites))
        call select_elites(population, fitness, elite_population, num_elites)

        ! オフスプリングの生成
        allocate(offspring(POPULATION_SIZE))

        ! エリート個体を次世代にコピー
        do i = 1, num_elites
            offspring(i)%ptr => elite_population(i)%ptr
        end do

        ! 残りの個体を生成
        !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i, r, parent_indices)
        do i = num_elites + 1, POPULATION_SIZE, 2
            call roulette_wheel_selection(fitness, parent_indices)
            offspring(i)%ptr => population(parent_indices(1))%ptr
            offspring(i + 1)%ptr => population(parent_indices(2))%ptr

            call random_number(r)
            if (r < CROSSOVER_RATE) then
                call subtree_crossover(offspring(i)%ptr, offspring(i + 1)%ptr)
            end if
        end do
        !$OMP END PARALLEL DO

        ! 突然変異
        !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i)
        do i = num_elites + 1, POPULATION_SIZE
            call mutate(offspring(i)%ptr)
        end do
        !$OMP END PARALLEL DO

        ! 次世代集団に更新
        deallocate(population)
        population = offspring
        deallocate(offspring)
        deallocate(elite_population)
    end do

    ! テストデータでの評価
    test_accuracy = evaluate_individual(best_individual, X_test, y_test, num_test)
    print *, "Test Accuracy of Best Individual: ", test_accuracy

contains

    subroutine roulette_wheel_selection(fitness, selected_indices)
        use parameters
        implicit none
        real, intent(in) :: fitness(:)
        integer, intent(out) :: selected_indices(2)
        integer :: i
        real :: total_fitness, cumulative_fitness(size(fitness))
        real :: r

        ! 累積適応度を計算
        total_fitness = sum(fitness)
        cumulative_fitness(1) = fitness(1)
        do i = 2, size(fitness)
            cumulative_fitness(i) = cumulative_fitness(i - 1) + fitness(i)
        end do

        ! 親個体の選択
        do i = 1, 2
            call random_number(r)
            r = r * total_fitness
            selected_indices(i) = 1
            do while (cumulative_fitness(selected_indices(i)) < r)
                selected_indices(i) = selected_indices(i) + 1
            end do
        end do
    end subroutine roulette_wheel_selection

    subroutine select_elites(population, fitness, elite_population, num_elites)
        use parameters
        implicit none
        type(TreeNodePointer), intent(in) :: population(:)
        real, intent(in) :: fitness(:)
        type(TreeNodePointer), intent(out) :: elite_population(:)
        integer, intent(in) :: num_elites
        integer :: i, j
        integer, allocatable :: elite_indices(:)
        ! real :: temp_fitness
        integer :: temp_index

        allocate(elite_indices(size(fitness)))
        do i = 1, size(fitness)
            elite_indices(i) = i
        end do

        ! 適応度でソート（降順）
        do i = 1, size(fitness) - 1
            do j = i + 1, size(fitness)
                if (fitness(elite_indices(j)) > fitness(elite_indices(i))) then
                    temp_index = elite_indices(i)
                    elite_indices(i) = elite_indices(j)
                    elite_indices(j) = temp_index
                end if
            end do
        end do

        ! 上位 num_elites 個を選択
        do i = 1, num_elites
            elite_population(i)%ptr => population(elite_indices(i))%ptr
        end do

        deallocate(elite_indices)
    end subroutine select_elites

end program genetic_algorithm_main
