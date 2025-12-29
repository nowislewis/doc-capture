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

;; 使用 org-capture 模板处理文档查看器捕获的内容。
;; 外部脚本调用 doc-capture-process，内部通过 org-capture 完成捕获。
;; 支持多种文档格式：PDF, EPUB, DJVU, XPS, CBZ 等。
;; 支持多种查看器：zathura, mupdf, evince, okular, pdf-tools 等。

;;; Code:

(require 'org)
(require 'org-capture)

;;; 自定义变量

(defgroup doc-capture nil
  "Document capture to org files."
  :group 'org)

(defcustom doc-capture-supported-extensions
  '("pdf" "epub" "djvu" "xps" "cbz" "cbr" "mobi" "azw" "azw3" "fb2" "ps")
  "支持的文档格式扩展名列表。"
  :type '(repeat string)
  :group 'doc-capture)

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
nil 表示与文档同目录，否则统一保存到指定目录。"
  :type '(choice (const :tag "Same as document directory" nil)
                 (directory :tag "Custom directory"))
  :group 'doc-capture)

(defcustom doc-capture-template-key "c"
  "org-capture 模板的 key。"
  :type 'string
  :group 'doc-capture)

;;; 内部变量

(defvar doc-capture--context nil
  "当前捕获上下文 (plist): (:file FILE :page PAGE :text TEXT).")

;;; 核心函数

(defun doc-capture-open (file-path page-num)
  "用配置的查看器打开 FILE-PATH 并跳转到 PAGE-NUM。"
  (let ((page-str (format "%s" page-num)))
    (pcase doc-capture-viewer
      ('pdf-tools
       (find-file file-path)
       (pdf-view-goto-page (string-to-number page-str)))
      ('custom
     (if doc-capture-viewer-command
           (let ((cmd (thread-last doc-capture-viewer-command
                        (replace-regexp-in-string "%f" file-path)
                        (replace-regexp-in-string "%p" page-str))))
             (start-process-shell-command "doc-viewer" nil cmd))
       (error "未设置 doc-capture-viewer-command")))
      (_
       (let ((args (pcase doc-capture-viewer
                     ('zathura (list "zathura" "--page" page-str file-path))
                     ('mupdf (list "mupdf" file-path page-str))
                     ('evince (list "evince" "--page-label" page-str file-path))
                     ('okular (list "okular" "--page" page-str file-path))
                     (_ (error "未知查看器: %s" doc-capture-viewer)))))
         (apply #'start-process "doc-viewer" nil args))))))

;;; org-capture 模板辅助函数

(defun doc-capture--get-target-file ()
  "计算目标 org 文件路径，自动创建文件头。"
  (unless doc-capture--context
    (error "doc-capture--context 为空"))
  (let* ((file-path (plist-get doc-capture--context :file))
         (file-base (file-name-sans-extension (file-name-nondirectory file-path)))
         (org-dir (or (and doc-capture-org-directory
                           (expand-file-name doc-capture-org-directory))
                      (file-name-directory file-path)))
         (org-file (expand-file-name (concat file-base ".org") org-dir)))
    (unless (file-directory-p org-dir)
      (make-directory org-dir t))
    (unless (file-exists-p org-file)
      (with-temp-file org-file
        (insert (format "#+TITLE: %s\n#+AUTHOR: \n#+DATE: %s\n\n* Notes\n\n"
                        file-base
                        (format-time-string "%Y-%m-%d %H:%M")))))
    org-file))

(defun doc-capture--get-page ()
  "返回当前页码字符串。"
  (format "%s" (plist-get doc-capture--context :page)))

(defun doc-capture--get-text ()
  "返回捕获的文本，空文本返回空字符串。"
  (or (plist-get doc-capture--context :text) ""))

(defun doc-capture--get-link ()
  "生成 elisp 链接，点击后返回文档对应页。"
  (let ((file-path (plist-get doc-capture--context :file))
        (page-num (plist-get doc-capture--context :page)))
    (format "[[elisp:(doc-capture-open \"%s\" %s)][%s]]"
            file-path page-num (file-name-nondirectory file-path))))

;;; 主入口函数

(defun doc-capture-process (file-path page-num selected-text &optional _)
  "外部脚本调用入口，触发 org-capture。
FILE-PATH: 文档路径
PAGE-NUM: 页码
SELECTED-TEXT: 选中的文本"
  (setq doc-capture--context
        (list :file file-path :page page-num :text selected-text))
  (org-capture nil doc-capture-template-key)
  (run-with-timer 0.1 nil (lambda () (setq doc-capture--context nil))))

;;; Hook

(defvar doc-capture-before-finalize-hook nil
  "在 doc-capture 完成前运行的 hook。
在 org-capture finalize 之前调用，可用于添加额外处理（如 org-inc 集成）。")

(defun doc-capture--run-hooks ()
  "运行 doc-capture 的 finalize hook。"
  (run-hooks 'doc-capture-before-finalize-hook))

;;; 设置 org-capture 模板

(defun doc-capture--setup-template ()
  "注册 org-capture 模板。"
  (setq org-capture-templates
        (cl-remove-if (lambda (tmpl)
                        (string= (car tmpl) doc-capture-template-key))
                      org-capture-templates))
  (add-to-list 'org-capture-templates
               `(,doc-capture-template-key
                 "Document Capture"
                 entry
                 (file+headline doc-capture--get-target-file "Notes")
                 "** Page %(doc-capture--get-page)\n%(doc-capture--get-text)\n\n%(doc-capture--get-link)\n"
                 :empty-lines 1
                 :immediate-finish t
                 :before-finalize (doc-capture--run-hooks))
               t))

(with-eval-after-load 'org-capture
  (doc-capture--setup-template))

(when (featurep 'org-capture)
  (doc-capture--setup-template))

(dolist (ext doc-capture-supported-extensions)
  (add-to-list 'org-file-apps (cons (concat "\\." ext "\\'") 'default)))

(provide 'doc-capture)
;;; doc-capture.el ends here
