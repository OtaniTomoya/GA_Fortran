module parameters
    implicit none
    ! mnist
!    integer, parameter :: POPULATION_SIZE = 101! 集団サイズ
!    integer, parameter :: GENERATIONS = 10000    ! 世代数
!    integer, parameter :: MIN_DEPTH = 10        ! ツリーの最小深さ
!    integer, parameter :: MAX_DEPTH = 18        ! ツリーの最大深さ
!    integer, parameter :: TOURNAMENT_SIZE = 0   ! トーナメント選択のサイズ
!    real, parameter    :: MUTATION_RATE = 0.01  ! 突然変異率
!    real, parameter    :: CROSSOVER_RATE = 0.5  ! 交叉率
!    integer, parameter :: NUM_FEATURES = 784    ! 特徴量の数（MNISTデータセット）
!    integer, parameter :: NUM_THRESHOLD = 255    ! 閾値(mnistデータセット)
!    integer, parameter :: NUM_CLASSES = 10      ! クラス数（0〜9）
!    integer, parameter :: SEED_VALUE = 215013
!    integer, parameter :: NUM_TRAIN = 60000
!    integer, parameter :: NUM_TEST = 10000
!    character(len=*), parameter :: train_file = 'mnist_train.csv'
!    character(len=*), parameter :: test_file = 'mnist_test.csv'

    ! digits
    integer, parameter :: POPULATION_SIZE = 10001 ! 集団サイズ
    integer, parameter :: GENERATIONS = 100000   ! 世代数
    integer, parameter :: MIN_DEPTH = 1        ! ツリーの最小深さ
    integer, parameter :: MAX_DEPTH = 14        ! ツリーの最大深さ
    integer, parameter :: TOURNAMENT_SIZE = 0   ! トーナメント選択のサイズ
    real, parameter    :: MUTATION_RATE = 0.02  ! 突然変異率
    real, parameter    :: CROSSOVER_RATE = 0.5  ! 交叉率
    integer, parameter :: NUM_FEATURES = 64    ! 特徴量の数
    integer, parameter :: NUM_THRESHOLD = 15    ! 閾値
    integer, parameter :: NUM_CLASSES = 10      ! クラス数（0〜9）
    integer, parameter :: SEED_VALUE = 215013
    integer, parameter :: NUM_TRAIN = 1437  ! データのサイズ
    integer, parameter :: NUM_TEST = 360
    character(len=*), parameter :: train_file = 'digits_train.csv'
    character(len=*), parameter :: test_file = 'digits_test.csv'
end module parameters

