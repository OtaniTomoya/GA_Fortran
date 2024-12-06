import numpy as np

# ファイル名とデータの型・サイズを指定
file_name = "data/data_20241206_031215.bin"
data_type = np.float32
data_size = 5

# バイナリデータの読み込み
gen, fitness = np.fromfile(file_name, dtype=data_type, count=data_size)

print("読み込んだデータ:", fitness)