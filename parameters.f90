module parameters
    implicit none
    integer, parameter :: POPULATION_SIZE = 10000! 集団サイズ
    integer, parameter :: GENERATIONS = 100     ! 世代数
    integer, parameter :: MIN_DEPTH = 10        ! ツリーの最小深さ
    integer, parameter :: MAX_DEPTH = 44        ! ツリーの最大深さ
    integer, parameter :: TOURNAMENT_SIZE = 3   ! トーナメント選択のサイズ
    real, parameter    :: MUTATION_RATE = 0.01  ! 突然変異率
    real, parameter    :: CROSSOVER_RATE = 0.5  ! 交叉率
    integer, parameter :: NUM_FEATURES = 784    ! 特徴量の数（MNISTデータセット）
    integer, parameter :: NUM_CLASSES = 10      ! クラス数（0〜9）
    integer, parameter :: MAX_NODES = 10000     ! ツリー内の最大ノード数
    integer, parameter :: SEED_VALUE = 215013   ! 乱数の種（名前を変更）
end module parameters

