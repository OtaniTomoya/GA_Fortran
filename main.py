import subprocess
import matplotlib.pyplot as plt
import requests
import pandas as pd
import glob
import time

def build_fortran():
    subprocess.run(["make"], check=True)

def run_fortran():
    subprocess.run(["./genetic_algorithm"], check=True)

def read_generation_data():
    csv_files = glob.glob("log/generation_data*.csv")
    if not csv_files:
        print("CSV file not found!")
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

    df['Generation'] = pd.to_numeric(df['Generation'], errors='coerce')
    df = df.dropna()
    df['Generation'] = df['Generation'].astype(int)

    plt.figure(figsize=(10, 6))
    plt.plot(df['Generation'], df['Max Fitness'], label='Max Fitness', linewidth=2)
    plt.plot(df['Generation'], df['Mean Fitness'], label='Mean Fitness', linewidth=2, linestyle='--')

    plt.title('GAによる決定木の最適化過程', fontsize=16)
    plt.xlabel('世代', fontsize=14)
    plt.ylabel('適応度', fontsize=14)
    plt.legend(fontsize=12)
    plt.grid(True)

    # 世代数が多い場合は軸のラベル設定を適宜調整
    # plt.xticks(...)

    # グラフ上にテスト精度を書く例
    if test_accuracy is not None:
        plt.text(
            x=int(df['Generation'].max()) // 2,  
            y=(df['Max Fitness'].max())//2,
            s=f"test accuracy: {test_accuracy:.1f}%", 
            fontsize=14,
            color='black',
            fontweight='bold',
            ha='center'
        )
    plt.savefig("fitness_plot.png")
    plt.close()

def send_image_to_discord(webhook_url, image_path, message=None):
    files = {"file": open(image_path, "rb")}
    data = {}
    if message is not None:
        data["content"] = message
    requests.post(webhook_url, files=files, data=data)

def read_parameters(filepath):
    """
    parametar.f90 などから、parameter宣言のみを抽出して返す。
    コメント行やmodule宣言など不要な行はスキップ。
    """
    extracted_lines = []
    with open(filepath, "r", encoding="utf-8") as f:
        for line in f:
            line_strip = line.strip()
            # 空行 or コメント行(!で始まる) はスキップ
            if not line_strip or line_strip.startswith('!'):
                continue
            # 'module', 'end module', 'implicit none' を含む行もスキップ
            lower_line = line_strip.lower()
            if ('module' in lower_line or
                'end module' in lower_line or
                'implicit none' in lower_line):
                continue

            # パラメータ以外にも設定が入っているかもしれないので、
            # "parameter ::" を含む行だけを対象とする (大文字小文字は一応無視)
            if 'parameter ::' not in lower_line:
                continue

            # 上記条件をすべてパスしたら、この行は「パラメータ宣言行」とみなす
            extracted_lines.append(line_strip)

    return extracted_lines

def main():
    webhook_url = "https://discord.com/api/webhooks/1319605518360383581/RWHm4qiVJC_6TuyS21j6JF2a5sKat-SOyctXf3bvMXuy2cqO8DQpKwW7W1sdPLGNMohX"
    start = time.time() 
    build_fortran()
    run_fortran()
    df, best_test_acc = read_generation_data()
    end = time.time() 

    if df is not None:
        plot_fitness(df, best_test_acc)

        # パラメータファイルの中身を読み込み
        param_lines = read_parameters("parameters.f90")
        param_text = "\n".join(param_lines)  # リストを改行で結合して文字列にする

        if best_test_acc is not None:
            text_for_discord = f"Best test accuracy: {best_test_acc}%\n実行時間: {str(end - start)}s"
        else:
            text_for_discord = ""

        text_for_discord += "```\n" + param_text + "\n```"

        send_image_to_discord(
            webhook_url,
            "fitness_plot.png",
            message=text_for_discord
        )
    else:
        print("No valid data in CSV -> no plot or param post.")

if __name__ == "__main__":
    main()
