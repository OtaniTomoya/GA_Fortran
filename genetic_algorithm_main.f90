program genetic_algorithm_main
    use parameters
    use data_handling
    use tree_generation
    use evaluation
    use genetic_operators
    use tree_structure
    use prediction
    implicit none

    real, allocatable :: X_train(:,:), X_test(:,:)
    integer, allocatable :: y_train(:), y_test(:)
    integer :: num_train, num_test
    type(TreeNode), pointer, allocatable :: population(:)
    real, allocatable :: fitness(:)
    integer :: i, j, generation
    real :: best_fitness, test_accuracy
    type(TreeNode), pointer :: best_individual
    type(TreeNode), pointer, allocatable :: offspring(:)
    integer :: parent_indices(2)
    real :: r

    ! 乱数の初期化
    call random_seed(RANDOM_SEED)

    ! データの読み込み
    call load_and_prepare_mnist(X_train, y_train, num_train, X_test, y_test, num_test)

    ! 初期集団の生成
    allocate(population(POPULATION_SIZE))
    allocate(fitness(POPULATION_SIZE))
    do i = 1, POPULATION_SIZE
        call create_random_tree(int(MIN_DEPTH + (MAX_DEPTH - MIN_DEPTH) * random_number()), 0, population(i))
    end do

    ! 世代ループ
    do generation = 1, GENERATIONS
        ! 適応度の計算
        do i = 1, POPULATION_SIZE
            fitness(i) = evaluate_individual(population(i), X_train, y_train, num_train)
        end do

        ! 最良個体の選択
        best_fitness = maxval(fitness)
        do i = 1, POPULATION_SIZE
            if (fitness(i) == best_fitness) then
                best_individual => population(i)
                exit
            end if
        end do

        print *, "Generation ", generation, ", Best Fitness (Training Accuracy): ", best_fitness

        ! オフスプリングの生成
        allocate(offspring(POPULATION_SIZE))

        ! 選択と交叉
        do i = 1, POPULATION_SIZE, 2
            call tournament_selection(fitness, parent_indices)
            offspring(i) => population(parent_indices(1))
            offspring(i + 1) => population(parent_indices(2))

            call random_number(r)
            if (r < CROSSOVER_RATE) then
                call subtree_crossover(offspring(i), offspring(i + 1))
            end if
        end do

        ! 突然変異
        do i = 1, POPULATION_SIZE
            call mutate(offspring(i))
        end do

        ! 次世代集団に更新
        population = offspring
        deallocate(offspring)
    end do

    ! テストデータでの評価
    test_accuracy = evaluate_individual(best_individual, X_test, y_test, num_test)
    print *, "Test Accuracy of Best Individual: ", test_accuracy

    ! メモリの解放
    ! 省略

end program genetic_algorithm_main

subroutine tournament_selection(fitness, selected_indices)
    use parameters
    implicit none
    real, intent(in) :: fitness(:)
    integer, intent(out) :: selected_indices(2)
    integer :: i, idx, best_idx
    real :: best_fitness, r

    do i = 1, 2
        best_fitness = -1.0
        do j = 1, TOURNAMENT_SIZE
            call random_number(r)
            idx = int(r * POPULATION_SIZE) + 1
            if (fitness(idx) > best_fitness) then
                best_fitness = fitness(idx)
                best_idx = idx
            end if
        end do
        selected_indices(i) = best_idx
    end do
end subroutine tournament_selection
