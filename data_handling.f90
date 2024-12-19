module data_handling
    use parameters
    implicit none
contains

    subroutine load_and_prepare_mnist(X_train, y_train, X_test, y_test)
        implicit none
        integer, intent(out) :: X_train(NUM_FEATURES, NUM_TRAIN), X_test(NUM_FEATURES, NUM_TEST)
        integer, intent(out) :: y_train(NUM_TRAIN), y_test(NUM_TRAIN)

        ! トレーニングデータの読み込み
        call read_mnist_data(train_file, X_train, y_train, NUM_TRAIN)

        ! テストデータの読み込み
        call read_mnist_data(test_file, X_test, y_test, NUM_TEST)

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
