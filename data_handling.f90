module data_handling
    use parameters
    implicit none
    contains

    subroutine load_and_prepare_mnist(X_train, y_train, num_train, X_test, y_test, num_test)
        implicit none
        real, allocatable, intent(out) :: X_train(:,:), X_test(:,:)
        integer, allocatable, intent(out) :: y_train(:), y_test(:)
        integer, intent(out) :: num_train, num_test

        ! データの読み込みと前処理を実装します。
        ! MNISTデータセットをCSVファイルから読み込むと仮定します。

        ! ファイル名
        character(len=*), parameter :: train_file = 'mnist_train.csv'
        character(len=*), parameter :: test_file = 'mnist_test.csv'

        ! データのサイズを設定（MNISTデータセットの場合）
        integer, parameter :: train_samples = 60000
        integer, parameter :: test_samples = 10000

        num_train = train_samples
        num_test = test_samples

        allocate(X_train(NUM_FEATURES, num_train))
        allocate(y_train(num_train))
        allocate(X_test(NUM_FEATURES, num_test))
        allocate(y_test(num_test))

        ! トレーニングデータの読み込み
        call read_mnist_data(train_file, X_train, y_train, num_train)

        ! テストデータの読み込み
        call read_mnist_data(test_file, X_test, y_test, num_test)

        ! データの標準化（平均0、標準偏差1）
        call standardize_data(X_train, num_train)
        call standardize_data(X_test, num_test)
    end subroutine load_and_prepare_mnist

    subroutine read_mnist_data(filename, X, y, num_samples)
        use parameters
        implicit none
        character(len=*), intent(in) :: filename
        real, intent(out) :: X(NUM_FEATURES, num_samples)
        integer, intent(out) :: y(num_samples)
        integer, intent(in) :: num_samples
        integer :: i, j, ios
        character(len=10000) :: line
        character(len=20), allocatable :: tokens(:)
        integer :: num_tokens
        real :: temp

        open(unit=10, file=filename, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'Error opening file: ', filename
            stop
        end if

        do i = 1, num_samples
            read(10, '(A)', iostat=ios) line
            if (ios /= 0) then
                print *, 'Error reading line ', i, ' from file: ', filename
                stop
            end if

            call split_line(line, ',', tokens, num_tokens)

            ! 最初の要素がラベル
            read(tokens(1), *) y(i)

            ! 残りが特徴量
            do j = 1, NUM_FEATURES
                read(tokens(j + 1), *) temp
                X(j, i) = temp
            end do
        end do

        close(10)
    end subroutine read_mnist_data

    subroutine split_line(line, delimiter, tokens, num_tokens)
        implicit none
        character(len=*), intent(in) :: line
        character(len=1), intent(in) :: delimiter
        character(len=*), allocatable, intent(out) :: tokens(:)
        integer, intent(out) :: num_tokens
        integer :: i, start, end_pos

        num_tokens = 0
        start = 1
        do i = 1, len(line)
            if (line(i:i) == delimiter .or. i == len(line)) then
                if (i == len(line)) then
                    end_pos = i
                else
                    end_pos = i - 1
                end if
                num_tokens = num_tokens + 1
                if (.not. allocated(tokens)) then
                    allocate(character(len=end_pos - start + 1) :: tokens(num_tokens))
                else
                    allocate(character(len=end_pos - start + 1) :: tokens_tmp(num_tokens))
                    tokens_tmp(1:num_tokens - 1) = tokens
                    deallocate(tokens)
                    tokens = tokens_tmp
                end if
                tokens(num_tokens) = line(start:end_pos)
                start = i + 1
            end if
        end do
    end subroutine split_line

    subroutine standardize_data(X, num_samples)
        implicit none
        real, intent(inout) :: X(:, :)
        integer, intent(in) :: num_samples
        integer :: i
        real :: mean_val, std_val
        real, allocatable :: feature(:)

        do i = 1, size(X, 1)
            feature = X(i, :)
            mean_val = sum(feature) / num_samples
            std_val = sqrt(sum((feature - mean_val) ** 2) / num_samples)
            if (std_val /= 0.0) then
                X(i, :) = (feature - mean_val) / std_val
            else
                X(i, :) = 0.0
            end if
            deallocate(feature)
        end do
    end subroutine standardize_data
end module data_handling
