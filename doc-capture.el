;;; doc-capture.el --- Emacs 函数处理文档查看器捕获的内容 -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: 
;; Package-Requires: ((org "9.0"))
;; Keywords: org, capture, pdf, document

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 这个文件包含了处理文档查看器捕获内容的函数。
;; 主要功能是自动将选中的文本保存到同名 org 文件中。
;; 支持多种文档格式：PDF, EPUB, DJVU, XPS, CBZ 等。
;; 支持多种查看器：zathura, mupdf, evince, okular, pdf-tools 等。

;;; Code:

(require 'org)

;; 支持的文档格式
(defcustom doc-capture-supported-extensions
  '("pdf" "epub" "djvu" "xps" "cbz" "cbr" "mobi" "azw" "azw3" "fb2" "ps")
  "支持的文档格式扩展名列表。"
  :type '(repeat string)
  :group 'doc-capture)

;; 可配置的文档查看器设置
(defcustom doc-capture-viewer 'zathura
  "文档查看器类型。
可选值：
  'zathura  - 使用 zathura (支持 PDF, EPUB, DJVU, PS, CBZ)
  'mupdf    - 使用 mupdf (支持 PDF, EPUB, XPS, CBZ, FB2)
  'evince   - 使用 evince (支持 PDF, DJVU, PS, XPS)
  'okular   - 使用 okular (支持 PDF, EPUB, DJVU, MOBI 等)
  'pdf-tools - 使用 Emacs pdf-tools (仅 PDF)
  'custom   - 使用自定义命令（需设置 doc-capture-viewer-command）"
  :type '(choice (const :tag "Zathura" zathura)
                 (const :tag "MuPDF" mupdf)
                 (const :tag "Evince" evince)
                 (const :tag "Okular" okular)
                 (const :tag "PDF Tools" pdf-tools)
                 (const :tag "Custom" custom))
  :group 'doc-capture)

(defcustom doc-capture-viewer-command nil
  "自定义文档查看器命令。
当 doc-capture-viewer 设置为 'custom 时使用。
命令中可以使用以下占位符：
  %f - 文件路径
  %p - 页码
示例：\"ebook-viewer %f\"
     \"mupdf %f %p\""
  :type '(choice (const :tag "None" nil)
                 (string :tag "Custom command"))
  :group 'doc-capture)

(defcustom doc-capture-org-directory nil
  "存储 org 笔记文件的目录。
如果设置为 nil，则 org 文件保存在文档文件的同一目录。
如果设置为目录路径，则所有 org 文件保存到该目录。
示例：\"~/Documents/reading-notes/\""
  :type '(choice (const :tag "Same as document directory" nil)
                 (directory :tag "Custom directory"))
  :group 'doc-capture)

(defvar doc-capture-post-process-hook nil
  "文档捕获完成后的处理钩子。
该钩子会在内容添加到 org 文件后被调用，可以用于自定义格式化或其他处理。
每个钩子函数接受三个参数：
  - ORG-FILE-PATH: org 文件路径
  - PAGE-NUM: 页码  
  - SELECTED-TEXT: 选中的文本
  - HEADING: 生成的 heading 文本

示例：
  (defun my-doc-capture-handler (org-file page-num selected-text heading)
    \"对捕获的内容进行自定义处理\"
    (message \"处理捕获的内容\"))
  
  (add-hook 'doc-capture-post-process-hook 'my-doc-capture-handler)
  
要使用 org-inc，可以添加钩子函数：
  (add-hook 'doc-capture-post-process-hook
            (lambda (org-file page-num selected-text heading)
              (when (require 'org-inc nil t)
                (org-inc-convert-heading heading))))")

(defcustom doc-capture-enable-hook t
  "是否启用后处理钩子。
如果设置为 nil，则在文档捕获完成后不会执行钩子函数。"
  :type 'boolean
  :group 'doc-capture)

(defun doc-capture-open (file-path page-num)
  "用配置的查看器打开 FILE-PATH 并跳转到 PAGE-NUM。"
  (pcase doc-capture-viewer
    ('zathura
     (start-process "doc-viewer" nil "zathura" "--page" (format "%s" page-num) file-path))
    ('mupdf

     ;; mupdf 页码从 1 开始，不需要特殊处理
     (start-process "doc-viewer" nil "mupdf" file-path (format "%s" page-num)))
    ('evince
     (start-process "doc-viewer" nil "evince" "--page-label" (format "%s" page-num) file-path))
    ('okular
     (start-process "doc-viewer" nil "okular" "--page" (format "%s" page-num) file-path))
    ('pdf-tools
     (find-file file-path)
     (pdf-view-goto-page (string-to-number (format "%s" page-num))))
    ('custom
     (if doc-capture-viewer-command
         (let* ((cmd (replace-regexp-in-string "%f" file-path doc-capture-viewer-command))
                (cmd (replace-regexp-in-string "%p" (format "%s" page-num) cmd)))
           (start-process-shell-command "doc-viewer" nil cmd))
       (error "未设置 doc-capture-viewer-command")))
    (_
     (error "未知的文档查看器类型: %s" doc-capture-viewer))))

;; 为不同文档格式设置打开方式
(dolist (ext doc-capture-supported-extensions)
  (add-to-list 'org-file-apps (cons (concat "\\." ext "\\'") 'default)))

(defun doc-capture-process (file-path page-num selected-text org-file-path)
  "处理文档捕获的内容。
FILE-PATH: 原始文件路径
PAGE-NUM: 页码
SELECTED-TEXT: 选中的文本
ORG-FILE-PATH: org 文件路径（已废弃，将自动计算）"
  (let* ((file-dir (file-name-directory file-path))
         (file-name (file-name-nondirectory file-path))
         (file-base (file-name-sans-extension file-name))
         (file-ext (file-name-extension file-name))
         ;; 根据配置决定 org 文件的保存位置
         (org-dir (if doc-capture-org-directory
                      (expand-file-name doc-capture-org-directory)
                    file-dir))
         (org-file (expand-file-name (concat file-base ".org") org-dir))
         (org-buffer nil))
    
    ;; 确保目标目录存在
    (unless (file-directory-p org-dir)
      (make-directory org-dir t))
    
    ;; 检查文件是否是新建的
    (let ((is-new-file (not (file-exists-p org-file))))
      
      ;; 打开或创建 org 文件（静默，不提示）
      (setq org-buffer (or (find-buffer-visiting org-file)
                          (let ((buf (create-file-buffer org-file)))
                            (with-current-buffer buf
                              (setq buffer-file-name org-file)
                              (when (file-exists-p org-file)
                                (insert-file-contents org-file))
                              (org-mode)
                              (setq buffer-file-coding-system 'utf-8-unix))
                            buf)))
      
      ;; 确保 org 文件存在并设置内容
      (with-current-buffer org-buffer
        ;; 如果文件是空的，添加头部信息
        (when (= (buffer-size) 0)
          (insert "#+TITLE: " file-base "\n")
          (insert "#+AUTHOR: \n")
          (insert "#+DATE: " (format-time-string "%Y-%m-%d %H:%M") "\n\n")
          (insert "* Notes\n\n"))
        
        ;; 切换到 Notes 部分
        (goto-char (point-max))
        
        ;; 如果当前不在 Notes 部分，导航到那里
        (unless (re-search-backward "^* Notes" nil t)
          (goto-char (point-max)))
        
        ;; 移动到 Notes 部分末尾
        (goto-char (point-max))
        
        ;; 添加新的 entry
        (insert "** Page " page-num "\n")
        
        ;; 添加选中的文本（如果有）
        (when (and selected-text (not (string-empty-p selected-text)))
          (insert selected-text "\n\n"))
        
        ;; 添加链接 - 使用统一的打开函数
        (insert "[[elisp:(doc-capture-open \"" file-path "\" " page-num ")][" file-name "]]\n")
        (insert "\n")
        
        ;; 执行后处理钩子（在保存之前，在当前 buffer 中）
        (when (and doc-capture-enable-hook
                   (boundp 'doc-capture-post-process-hook)
                   doc-capture-post-process-hook)
          (let* ((heading (concat "** Page " page-num))
                 (selected-text-safe (or selected-text "")))
            ;; 按顺序执行所有钩子函数
            (dolist (hook-function doc-capture-post-process-hook)
              (when (and hook-function (fboundp hook-function))
                (condition-case err
                    (funcall hook-function org-file page-num selected-text-safe heading)
                  (error
                   (message "doc-capture 钩子函数 %s 执行失败: %s" 
                           hook-function
                           (error-message-string err))))))))
        
        ;; 保存文件（静默，不提示）- 在 hook 执行之后
        (write-region (point-min) (point-max) org-file nil 'silent)
        (set-buffer-modified-p nil))
      
      ;; 返回消息，说明是新建还是更新
      (if is-new-file
          (message "✓ 已创建新文件并捕获到 %s" org-file)
        (message "✓ 已捕获到 %s" org-file)))))

(provide 'doc-capture)
;;; doc-capture.el ends here
