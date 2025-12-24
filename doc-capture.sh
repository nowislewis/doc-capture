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

# 尝试从不同的 selection 获取文字
SELECTED_TEXT=$(xclip -out -selection primary 2>/dev/null)
if [ -z "$SELECTED_TEXT" ]; then
    SELECTED_TEXT=$(xclip -out -selection clipboard 2>/dev/null)
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

# 调用 emacs 函数处理（使用 batch 模式，非交互式）
# 注意：需要确保 doc-capture.el 在 Emacs load-path 中
emacs --batch --eval "
(progn
  (add-to-list 'load-path \"/home/lewisliu/Documents/code/doc-capture\")
  (require 'doc-capture)
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
    (doc-capture-process file-path page-num selected-text org-file-path)))" 2>&1 | tee -a "$DEBUG_LOG"

echo "Script completed" >> "$DEBUG_LOG"
