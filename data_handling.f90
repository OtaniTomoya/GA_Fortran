module data_handling
    use parameters
    implicit none
contains
! データの準備
    subroutine load_and_prepare_data(X_train, y_train, X_test, y_test)
        implicit none
        integer, intent(out) :: X_train(NUM_FEATURES, NUM_TRAIN)
        integer, intent(out) :: X_test(NUM_FEATURES, NUM_TEST)
        integer, intent(out) :: y_train(NUM_TRAIN), y_test(NUM_TRAIN)

        ! 訓練データの読み込み
        call read_data(train_file, X_train, y_train, NUM_TRAIN) 

        ! テストデータの読み込み
        call read_data(test_file, X_test, y_test, NUM_TEST)

    end subroutine load_and_prepare_data

! データの読み込み
    subroutine read_data(filename, X, y, num_samples)
        use parameters
        implicit none
        character(len=*), intent(in) :: filename
        integer, intent(out) :: X(NUM_FEATURES, num_samples)
        integer, intent(out) :: y(num_samples)
        integer, intent(in) :: num_samples
        integer :: i, j, ios
        integer :: unit
        ! ファイルのオープン
        open(newunit=unit,file=filename,status='old',action='read',iostat=ios)
        if (ios /= 0) then  ! エラー処理
            print *, 'Error opening file: ', filename
            stop
        end if

        do i = 1, num_samples 
            ! データの読み込み  
            read(unit, *, iostat=ios) y(i), (X(j, i), j = 1, NUM_FEATURES)

            if (ios /= 0) then  ! エラー処理
                print *,'Error reading data at line ',i,' from file: ',filename
                stop
            end if
        end do

        close(unit)
    end subroutine read_data

end module data_handling
