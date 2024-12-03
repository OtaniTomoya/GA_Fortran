module data_handling
    use parameters
    implicit none
contains

    subroutine load_and_prepare_mnist(X_train, y_train, num_train, X_test, y_test, num_test)
        implicit none
        real, allocatable, intent(out) :: X_train(:,:), X_test(:,:)
        integer, allocatable, intent(out) :: y_train(:), y_test(:)
        integer, intent(out) :: num_train, num_test

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
        integer :: unit

        ! ファイルを開く
        open(newunit=unit, file=filename, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'Error opening file: ', filename
            stop
        end if

        ! カンマ区切りのデータを読み込む
        do i = 1, num_samples
            read(unit, *, iostat=ios) y(i), (X(j, i), j = 1, NUM_FEATURES)
            if (ios /= 0) then
                print *, 'Error reading data at line ', i, ' from file: ', filename
                stop
            end if
        end do

        close(unit)
    end subroutine read_mnist_data

    subroutine standardize_data(X, num_samples)
        use parameters
        implicit none
        real, intent(inout) :: X(NUM_FEATURES, num_samples)
        integer, intent(in) :: num_samples
        integer :: i
        real :: mean_val, std_val
        real, allocatable :: feature(:)

        do i = 1, NUM_FEATURES
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
