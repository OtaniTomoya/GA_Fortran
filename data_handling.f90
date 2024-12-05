module data_handling
    use parameters
    implicit none
contains

    subroutine load_and_prepare_mnist(X_train, y_train, num_train, X_test, y_test, num_test)
        implicit none
        integer, allocatable, intent(out) :: X_train(:,:), X_test(:,:)
        integer, allocatable, intent(out) :: y_train(:), y_test(:)
        integer, intent(out) :: num_train, num_test

        ! ファイル名
        !character(len=*), parameter :: train_file = 'mnist_train.csv'
        !character(len=*), parameter :: test_file = 'mnist_test.csv'

        character(len=*), parameter :: train_file = 'digits_train.csv'
        character(len=*), parameter :: test_file = 'digits_test.csv'

        ! データのサイズを設定（MNISTデータセットの場合）
        !integer, parameter :: train_samples = 60000
        !integer, parameter :: test_samples = 10000

        integer, parameter :: train_samples = 1437
        integer, parameter :: test_samples = 360

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

    end subroutine load_and_prepare_mnist

    subroutine read_mnist_data(filename, X, y, num_samples)
        use parameters
        implicit none
        character(len=*), intent(in) :: filename
        integer, intent(out) :: X(NUM_FEATURES, num_samples)
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

end module data_handling
