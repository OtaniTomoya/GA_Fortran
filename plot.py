import pandas as pd
import matplotlib.pyplot as plt

df = pd.read_csv('/Users/tomoya/Code/GA_Fortran/log/generation_data-20241220225638 .csv')
df.columns = df.columns.str.strip()

# `test accuracy` の値を取得
test_accuracy_row = df.iloc[-1, 0]
if "test accuracy" in test_accuracy_row:
    test_accuracy = float(test_accuracy_row.split("test accuracy")[1].strip())
    df = df.iloc[:-1]  # 最後の行をデータフレームから削除
else:
    test_accuracy = None

# Generation列を数値型に変換
df['Generation'] = pd.to_numeric(df['Generation'], errors='coerce')

df = df.dropna()  # 無効な行を削除
df['Generation'] = df['Generation'].astype(int)

# プロット作成
plt.figure(figsize=(10, 6))
plt.plot(df['Generation'], df['Max Fitness'], label='Max Fitness', linewidth=2)
plt.plot(df['Generation'], df['Mean Fitness'], label='Mean Fitness', linewidth=2, linestyle='--')

# タイトルとラベルの設定
plt.title('GAによる決定木の最適化過程', fontsize=16)
plt.xlabel('世代', fontsize=14)
plt.ylabel('適応度', fontsize=14)

plt.xticks(range(0, int(df['Generation'].max()) + 1, int(int(df['Generation'].max())/10)))

if test_accuracy is not None:
    plt.text(
        x=int(df['Generation'].max()) // 2,  
        y=(df['Max Fitness'].max())//2,
        s=f"test accuracy: {test_accuracy:.1f}%", 
        fontsize=14,
        color='black',
        fontweight='bold',
        ha='center'  # 水平中央揃え
    )
plt.legend(fontsize=12)
plt.grid(True)
# プロットを表示
plt.show()