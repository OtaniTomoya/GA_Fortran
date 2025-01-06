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
    integer :: X_train(NUM_FEATURES, NUM_TRAIN), X_test(NUM_FEATURES, NUM_TEST)
    integer :: y_train(NUM_TRAIN), y_test(NUM_TRAIN)
    type(TreeNodePointer), allocatable :: population(:), offspring(:), temp_population(:)
    real, allocatable :: fitness(:)
    integer :: i, generation
    real :: best_fitness, mean_fitness
    integer :: parent_indices(2)
    real :: r, timer
    integer :: depth
    integer :: seed_size
    integer, allocatable :: seed(:)
    integer :: best_index, cnt_change
    logical, allocatable :: changed(:)
    integer :: time_begin_c,time_end_c, CountPerSec, CountMax
    REAL, parameter :: epsilon = 0.001
    character (Len=8) :: date 
    character (Len=10) :: time
    character (Len=50) :: filename
    
    call get_date_time(date, time)
    ! Log
    write(filename, '(A,"-",A," .csv")') "log/generation_data", trim(date)//trim(time)
    open(unit=10, file=trim(filename), status="unknown", action="write", position="append")
    write(10, '(A)') "Generation, Max Fitness, Mean Fitness"

    ! 乱数の初期化
    call random_seed(size=seed_size)
    allocate(seed(seed_size))
    seed = SEED_VALUE
    call random_seed(put=seed)

    ! データの読み込み
    call load_and_prepare_data(X_train, y_train, X_test, y_test)

    ! 初期集団の生成
    allocate(population(POPULATION_SIZE))
    allocate(fitness(POPULATION_SIZE))
    allocate(changed(POPULATION_SIZE))

    !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i, r, depth)
        do i = 1, POPULATION_SIZE
            call random_number(r)
            depth = int(MIN_DEPTH + (MAX_DEPTH - MIN_DEPTH) * r)
            call create_random_tree(depth, 0, population(i)%ptr)
            changed(i) = .true.
        end do
    !$OMP END PARALLEL DO

    ! オフスプリングの配列を最初に割り当て
    allocate(offspring(POPULATION_SIZE))
    print '(A)', "Generation     Ind       max       mean       time"
    ! 世代ループ
    do generation = 1, GENERATIONS
        call system_clock(time_begin_c, CountPerSec, CountMax)
        cnt_change = count(changed)
        ! オフスプリングの生成前に古いツリーを解放
        do i = 1, POPULATION_SIZE
            call deallocate_tree(offspring(i)%ptr)
        end do

        ! 適応度の計算
        !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i)
            do i = 1, POPULATION_SIZE
                if (changed(i)) then
                    fitness(i) = evaluate_individual(population(i)%ptr, X_train, y_train, num_train)
                    changed(i) = .false.
                end if
            end do
        !$OMP END PARALLEL DO

        best_fitness = maxval(fitness)
        mean_fitness = SUM(fitness) / SIZE(fitness)

        ! エリート保存
        best_index = maxloc(fitness, dim=1)
        call copy_tree(population(best_index)%ptr, offspring(1)%ptr)
        changed(1) = .true.

        best_fitness = maxval(fitness)
        mean_fitness = SUM(fitness) / SIZE(fitness)

        call output_generation_data(generation, best_fitness*100, mean_fitness*100)

        ! 選択と交叉
        do i = 2, POPULATION_SIZE, 2
            call roulette_wheel_selection(fitness, parent_indices)

            call copy_tree(population(parent_indices(1))%ptr, offspring(i)%ptr)
            call copy_tree(population(parent_indices(2))%ptr, offspring(i + 1)%ptr)

            call random_number(r)
            if (r < CROSSOVER_RATE) then
                changed(i) = .true.
                changed(i+1) = .true.
                call subtree_crossover(offspring(i)%ptr, offspring(i + 1)%ptr)
            end if
        end do

        ! 突然変異
        !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i)
            do i = 2, POPULATION_SIZE
                call random_number(r)
                if (r < MUTATION_RATE) then
                    changed(i) = .true.
                    call mutate(offspring(i)%ptr)
                end if
            end do
        !$OMP END PARALLEL DO

        ! 配列をスワップ
        call move_alloc(population, temp_population)
        call move_alloc(offspring, population)
        call move_alloc(temp_population, offspring)

        call system_clock(time_end_c)
        timer = real(time_end_c - time_begin_c)/CountPerSec
        print '(I10, I10, F10.2, F10.2, F10.2, F10.2)', generation, cnt_change, best_fitness*100, mean_fitness*100, timer, (GENERATIONS-generation)*timer*0.000277778
        if (best_fitness - mean_fitness < epsilon) then
            exit
        end if
    end do

    ! テストデータでの評価
    do i = 1, POPULATION_SIZE
        fitness(i) = pre(population(i)%ptr, X_test, y_test, num_test)
    end do
    print *, "Test Accuracy of Best Individual: ", maxval(fitness)*100
    print *, "Test Accuracy of Mean Individual: ", SUM(fitness) / SIZE(fitness) * 100
    print *, "Test Accuracy of Worst Individual: ", minval(fitness)*100

    write(10, '(A, F0.4)') "test accuracy", maxval(fitness)*100
    close(10)

    ! メモリの解放
    do i = 1, POPULATION_SIZE
        call deallocate_tree(population(i)%ptr)
        call deallocate_tree(offspring(i)%ptr)
    end do

    deallocate(population)
    deallocate(offspring)
    deallocate(fitness)
    deallocate(seed)

contains
    subroutine output_generation_data(generation, max_fitness, mean_fitness)
        integer, intent(in) :: generation 
        real, intent(in) :: max_fitness, mean_fitness

        ! ファイルにデータを書き込む（カンマ区切り）
        write(10, '(I0,",",F0.4,",",F0.4)') generation, max_fitness, mean_fitness
    end subroutine output_generation_data

    subroutine get_date_time(date, time)
        character (len=8), intent(out) :: date
        character (Len=10), intent(out) :: time
        integer :: values(8)
        call date_and_time (values=values)
        write(date, '(I4.4,I2.2,I2.2)') values(1), values(2), values (3)
        write(time, ' (I2.2,I2.2, I2.2)') values(5), values(6), values(7)
    end subroutine get_date_time

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
