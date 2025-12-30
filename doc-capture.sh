#!/bin/bash

# doc-capture.sh - 自动将文档查看器选中的内容捕获到同名 org 文件
# 支持多种文档格式：PDF, EPUB, DJVU, XPS, CBZ 等

FILE_PATH="$1"
PAGE_NUM="$2"

# 调试日志文件
DEBUG_LOG="/tmp/doc-capture-debug.log"

# 确保参数存在
if [ -z "$FILE_PATH" ] || [ -z "$PAGE_NUM" ]; then
    echo "Usage: $0 <FILE_PATH> <PAGE_NUM>" | tee -a "$DEBUG_LOG"
    exit 1
fi

# 记录调试信息
echo "=== $(date) ===" >> "$DEBUG_LOG"
echo "FILE_PATH: $FILE_PATH" >> "$DEBUG_LOG"
echo "PAGE_NUM: $PAGE_NUM" >> "$DEBUG_LOG"

# 尝试从剪贴板获取文字（支持 macOS 和 Linux）
if command -v pbpaste &> /dev/null; then
    # macOS 使用 pbpaste
    SELECTED_TEXT=$(pbpaste 2>/dev/null)
    echo "Using pbpaste (macOS)" >> "$DEBUG_LOG"
elif command -v xclip &> /dev/null; then
    # Linux 使用 xclip
    SELECTED_TEXT=$(xclip -out -selection primary 2>/dev/null)
    if [ -z "$SELECTED_TEXT" ]; then
        SELECTED_TEXT=$(xclip -out -selection clipboard 2>/dev/null)
    fi
    echo "Using xclip (Linux)" >> "$DEBUG_LOG"
else
    echo "Warning: No clipboard tool found (neither pbpaste nor xclip)" >> "$DEBUG_LOG"
    SELECTED_TEXT=""
fi

echo "SELECTED_TEXT length: ${#SELECTED_TEXT}" >> "$DEBUG_LOG"
echo "SELECTED_TEXT: $SELECTED_TEXT" >> "$DEBUG_LOG"

# 获取文件信息
FILE_DIR=$(dirname "$FILE_PATH")
FILE_NAME=$(basename "$FILE_PATH")
FILE_BASE="${FILE_NAME%.*}"
ORG_FILE="${FILE_DIR}/${FILE_BASE}.org"

echo "ORG_FILE: $ORG_FILE" >> "$DEBUG_LOG"

# 创建临时文件存储选中的文本
TEMP_FILE=$(mktemp)
echo "$SELECTED_TEXT" > "$TEMP_FILE"

echo "TEMP_FILE: $TEMP_FILE" >> "$DEBUG_LOG"
echo "TEMP_FILE content:" >> "$DEBUG_LOG"
cat "$TEMP_FILE" >> "$DEBUG_LOG"

# 检查 Emacs server 是否运行
if ! emacsclient --eval "t" &>/dev/null; then
    ERROR_MSG="❌ 错误: Emacs server 未运行。请在 Emacs 中执行 'M-x server-start' 或在配置中添加 (server-start)"
    echo "$ERROR_MSG" | tee -a "$DEBUG_LOG"
    notify-send "doc-capture 错误" "$ERROR_MSG"
    exit 1
fi

# 检查必需的函数和变量是否存在
CHECK_RESULT=$(emacsclient --eval "
(let ((errors '()))
  (unless (fboundp 'doc-capture-process)
    (push \"函数 'doc-capture-process' 未定义，请确保已加载 doc-capture.el\" errors))
  (unless (boundp 'doc-capture-org-directory)
    (push \"变量 'doc-capture-org-directory' 未定义，请检查配置\" errors))
  (if errors
      (mapconcat 'identity (reverse errors) \"; \")
    nil))" 2>&1)

if [ "$CHECK_RESULT" != "nil" ]; then
    ERROR_MSG="❌ doc-capture 配置错误: $CHECK_RESULT"
    echo "$ERROR_MSG" | tee -a "$DEBUG_LOG"
    notify-send "doc-capture 错误" "$ERROR_MSG"
    exit 1
fi

echo "✓ Emacs server 检查通过" >> "$DEBUG_LOG"

# 调用 emacs 函数处理（使用 emacsclient）
emacsclient --eval "
(let* ((file-path \"$FILE_PATH\")
       (page-num \"$PAGE_NUM\")
       (temp-file \"$TEMP_FILE\")
       (selected-text (when (file-exists-p temp-file)
                        (with-temp-buffer
                          (insert-file-contents temp-file)
                          (buffer-string))))
       (org-file-path \"$ORG_FILE\"))
  (when (file-exists-p temp-file)
    (delete-file temp-file))
  (doc-capture-process file-path page-num selected-text org-file-path))" 2>&1 | tee -a "$DEBUG_LOG"

echo "Script completed" >> "$DEBUG_LOG"
