import subprocess
import matplotlib.pyplot as plt
import requests
import pandas as pd
import glob

def build_fortran():
    """MakefileでFortranをビルド"""
    subprocess.run(["make"], check=True)

def run_fortran():
    """生成された実行ファイル(geneticalgorithm)を実行"""
    subprocess.run(["./genetic_algorithm"], check=True)


def read_generation_data():
    csv_files = glob.glob("log/generation_data*.csv")
    if not csv_files:
        print("No CSV file found!")
        return None, None

    csv_files.sort()
    newest_file = csv_files[-1]

    df = pd.read_csv(newest_file)

    test_accuracy = None
    if "test accuracy" in df.iloc[-1, 0]:  
        last_row_text = df.iloc[-1, 0]
        value_str = last_row_text.replace("test accuracy", "").strip()
        test_accuracy = float(value_str)
        df = df.iloc[:-1, :]

    return df, test_accuracy

def plot_fitness(df, test_accuracy):
    df.columns = df.columns.str.strip()

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
    plt.savefig("fitness_plot.png")



def send_image_to_discord(webhook_url, image_path, message=None):
    files = {"file": open(image_path, "rb")}
    data = {}
    if message is not None:
        data["content"] = message

    requests.post(webhook_url, files=files, data=data)

def main():
    webhook_url = "https://discord.com/api/webhooks/1319605518360383581/RWHm4qiVJC_6TuyS21j6JF2a5sKat-SOyctXf3bvMXuy2cqO8DQpKwW7W1sdPLGNMohX"  # 自分のWebhook URL

    # 1. Fortranをビルド
    build_fortran()

    run_fortran()
    df, best_test_acc = read_generation_data()

    # 3. 可視化
    plot_fitness(df, best_test_acc)

    # 4. Discordに画像送信
    send_image_to_discord(webhook_url, "fitness_plot.png",
                           message=f"Best test accuracy: {best_test_acc}")

if __name__ == "__main__":
    main()