# doc-capture Makefile
# 自动安装 zathura 配置文件和文档捕获脚本

# 默认安装前缀
PREFIX ?= $(HOME)
ZATHURA_CONFIG_DIR ?= $(PREFIX)/.config/zathura
ZATHURA_SCRIPTS_DIR ?= $(PREFIX)/.config/zathura/scripts

# 源文件
SOURCES = \
    doc-capture.sh \
    zathurarc

# 安装目标
INSTALL_TARGETS = \
    $(ZATHURA_SCRIPTS_DIR)/doc-capture.sh \
    $(ZATHURA_CONFIG_DIR)/zathurarc

# 默认目标
.PHONY: all
all: install

# 检查依赖
.PHONY: check
check:
	@echo "检查依赖..."
	@which zathura >/dev/null 2>&1 || { echo "警告: 未检测到 zathura"; }
	@which xclip >/dev/null 2>&1 || { echo "警告: 未检测到 xclip（用于剪贴板操作）"; }
	@which bash >/dev/null 2>&1 || { echo "错误: 需要安装 bash"; exit 1; }
	@echo "依赖检查完成"

# 创建目录
.PHONY: directories
directories:
	@echo "创建安装目录..."
	@mkdir -p $(ZATHURA_SCRIPTS_DIR)
	@mkdir -p $(ZATHURA_CONFIG_DIR)

# 备份现有配置文件
.PHONY: backup
backup:
	@echo "备份现有配置文件..."
	@if [ -f $(ZATHURA_CONFIG_DIR)/zathurarc ]; then \
		cp $(ZATHURA_CONFIG_DIR)/zathurarc $(ZATHURA_CONFIG_DIR)/zathurarc.backup.$(shell date +%Y%m%d_%H%M%S); \
		echo "已备份现有 zathurarc 文件"; \
	else \
		echo "未找到现有 zathurarc 文件，跳过备份"; \
	fi

# 安装文档捕获工具（包含备份）
.PHONY: install
install: backup check directories
	@echo "安装文档捕获工具..."
	@echo "安装 doc-capture.sh 到 $(ZATHURA_SCRIPTS_DIR)/"
	@install -m 755 doc-capture.sh $(ZATHURA_SCRIPTS_DIR)/doc-capture.sh
	@echo "安装 zathurarc 到 $(ZATHURA_CONFIG_DIR)/"
	@install -m 644 zathurarc $(ZATHURA_CONFIG_DIR)/zathurarc
	@echo ""
	@echo "安装完成!"
	@echo ""
	@echo "安装的文件:"
	@echo "- 脚本文件: $(ZATHURA_SCRIPTS_DIR)/doc-capture.sh"
	@echo "- Zathura 配置: $(ZATHURA_CONFIG_DIR)/zathurarc"
	@echo ""
	@echo "使用说明:"
	@echo "1. 在 Zathura 中打开 PDF/EPUB 等文档"
	@echo "2. 选中文本后按 Ctrl+C"
	@echo "3. 选中的内容会自动保存到同名 .org 文件中"

# 卸载
.PHONY: uninstall
uninstall:
	@echo "卸载文档捕获工具..."
	@rm -f $(ZATHURA_SCRIPTS_DIR)/doc-capture.sh
	@echo "脚本文件已删除"
	@if [ -f $(ZATHURA_CONFIG_DIR)/zathurarc ]; then \
		echo "备份并删除 zathurarc 文件..."; \
		cp $(ZATHURA_CONFIG_DIR)/zathurarc $(ZATHURA_CONFIG_DIR)/zathurarc.backup.$(shell date +%Y%m%d_%H%M%S); \
		rm -f $(ZATHURA_CONFIG_DIR)/zathurarc; \
		echo "zathurarc 已备份并删除"; \
	else \
		echo "zathurarc 文件不存在"; \
	fi
	@echo "卸载完成"

# 清理临时文件
.PHONY: clean
clean:
	@echo "清理..."
	@find . -name "*.elc" -delete
	@echo "清理完成"

# 显示安装状态
.PHONY: status
status:
	@echo "安装状态:"
	@echo "脚本文件: $(ZATHURA_SCRIPTS_DIR)/doc-capture.sh"
	@if [ -f $(ZATHURA_SCRIPTS_DIR)/doc-capture.sh ]; then \
		echo "  状态: 已安装"; \
	else \
		echo "  状态: 未安装"; \
	fi
	@echo "Zathura 配置: $(ZATHURA_CONFIG_DIR)/zathurarc"
	@if [ -f $(ZATHURA_CONFIG_DIR)/zathurarc ]; then \
		echo "  状态: 已安装"; \
	else \
		echo "  状态: 未安装"; \
	fi

# 显示帮助
.PHONY: help
help:
	@echo "doc-capture 安装工具"
	@echo ""
	@echo "可用目标:"
	@echo "  make install        - 安装工具（包含备份）"
	@echo "  make backup         - 备份现有配置文件"
	@echo "  make uninstall      - 卸载"
	@echo "  make clean          - 清理临时文件"
	@echo "  make status         - 显示安装状态"
	@echo "  make check          - 检查依赖"
	@echo "  make help           - 显示此帮助"
	@echo ""
	@echo "变量:"
	@echo "  PREFIX=$(PREFIX)                    - 安装前缀（默认: HOME）"
	@echo "  ZATHURA_CONFIG_DIR=$(ZATHURA_CONFIG_DIR) - Zathura 配置目录"
	@echo "  ZATHURA_SCRIPTS_DIR=$(ZATHURA_SCRIPTS_DIR) - Zathura 脚本目录"
	@echo ""
	@echo "示例:"
	@echo "  make install PREFIX=/opt    # 安装到 /opt 目录"
	@echo "  make install                # 标准安装"

# 指定文件优先级
doc-capture.sh:
zathurarc: