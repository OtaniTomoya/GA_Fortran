# GA_Fortran

intel コンパイラをつかえる場合は以下のようにコンテナを作る

```.devcontainer/devcontainer.json
{
    "name": "実験環境",
    "build": { "dockerfile": "Dockerfile" },
    "runArgs": ["--init","--name","fortran_workbench"],
    "customizations": {
    "vscode": {
    "settings": {
    "diffEditor.ignoreTrimWhitespace": false,
    "explorer.openEditors.visible": 0,
    "files.insertFinalNewline": true,
    "files.trimTrailingWhitespace": true,
    "markdown-preview-enhanced.scrollSync": false
    },
    "extensions": [
    "oderwat.indent-rainbow",
    "fortran-lang.linter-gfortran",
    "github.copilot"
    ]
    }
    }
    }
```
```.devcontainer/Dockerfile
FROM ubuntu:20.04

USER root

# 必要なディレクトリを作成
RUN mkdir -p /root/workspace
WORKDIR /root/workspace

# 非対話モードでの設定
ENV DEBIAN_FRONTEND=noninteractive

# 必要なツールをインストール
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    build-essential \
    gfortran \
    git \
    vim \
    tzdata \
    wget \
    gnupg \
    ca-certificates && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Intel OneAPIのGPGキーとリポジトリを設定
RUN wget -qO- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB | gpg --dearmor > /usr/share/keyrings/intel-oneapi-keyring.gpg && \
    echo "deb [signed-by=/usr/share/keyrings/intel-oneapi-keyring.gpg] https://apt.repos.intel.com/oneapi all main" > /etc/apt/sources.list.d/oneAPI.list

# Intel BasekitおよびHPCKitのインストール
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    intel-basekit \
    intel-hpckit && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Intel環境変数を設定
RUN echo "source /opt/intel/oneapi/setvars.sh" >> /etc/bash.bashrc

```
```makefile
# Makefile for the genetic algorithm Fortran project with OpenMP

# Compiler
FC = ifx

# Compiler flags
FFLAGS = -fopenmp -O1
#FFLAGS = -Wall -Wextra -fimplicit-none

# Module and object files
MODULES = parameters.mod data_handling.mod tree_structure.mod tree_generation.mod leaf_label_update.mod prediction.mod evaluation.mod genetic_operators.mod

OBJS = parameters.o data_handling.o tree_structure.o tree_generation.o leaf_label_update.o prediction.o evaluation.o genetic_operators.o genetic_algorithm_main.o

# Executable name
EXEC = genetic_algorithm

# Default target
all: $(EXEC)

# Link the executable
$(EXEC): $(OBJS)
	$(FC) $(FFLAGS) -o $(EXEC) $(OBJS)

# Compile modules and source files
parameters.o: parameters.f90
	$(FC) $(FFLAGS) -c parameters.f90

data_handling.o: data_handling.f90 parameters.mod
	$(FC) $(FFLAGS) -c data_handling.f90

tree_structure.o: tree_structure.f90 parameters.mod
	$(FC) $(FFLAGS) -c tree_structure.f90

tree_generation.o: tree_generation.f90 tree_structure.mod parameters.mod
	$(FC) $(FFLAGS) -c tree_generation.f90

leaf_label_update.o: leaf_label_update.f90 tree_structure.mod parameters.mod
	$(FC) $(FFLAGS) -c leaf_label_update.f90

prediction.o: prediction.f90 tree_structure.mod parameters.mod
	$(FC) $(FFLAGS) -c prediction.f90

evaluation.o: evaluation.f90 prediction.mod tree_structure.mod parameters.mod
	$(FC) $(FFLAGS) -c evaluation.f90

genetic_operators.o: genetic_operators.f90 tree_structure.mod parameters.mod tree_generation.mod leaf_label_update.mod
	$(FC) $(FFLAGS) -c genetic_operators.f90

genetic_algorithm_main.o: genetic_algorithm_main.f90 genetic_operators.mod tree_structure.mod parameters.mod evaluation.mod
	$(FC) $(FFLAGS) -c genetic_algorithm_main.f90

# Clean up
clean:
	rm -f $(OBJS) $(MODULES) $(EXEC)

# Phony targets
.PHONY: all clean

```
