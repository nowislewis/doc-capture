;;; doc-capture-org-inc-integration.el --- org-inc integration for doc-capture -*- lexical-binding: t; -*-

;; Author: Lewis Liu
;; Version: 1.0
;; Package-Requires: ((emacs "24.4") (doc-capture "1.0") (org-inc "1.0"))

;;; Commentary:
;; 提供 doc-capture 与 org-inc 的自动集成
;; 用法：(require 'doc-capture-org-inc-integration) 即可自动启用

;;; Code:

(require 'doc-capture)
(require 'org-inc)

;;; 配置选项

(defcustom doc-capture-org-inc-handler-type 'topic
  "org-inc 集成处理器类型。
- topic: 基础版本，适合新手
- smart: 智能版本，根据文档类型自动调整
- conditional: 条件版本，只对 PDF/电子书使用 org-inc
- nil: 禁用 org-inc 集成"
  :type '(choice (const :tag "基础版本" topic)
                 (const :tag "智能版本" smart)
                 (const :tag "条件版本" conditional)
                 (const :tag "禁用" nil))
  :group 'doc-capture)

;;; 辅助函数

(defun doc-capture-org-inc--ensure-notes-section ()
  "确保 Notes 部分存在，返回其末尾位置。"
  (goto-char (point-max))
  (unless (re-search-backward "^\\* Notes" nil t)
    (goto-char (point-max))
    (insert "* Notes\n\n"))
  (goto-char (point-max)))

(defun doc-capture-org-inc--create-topic (title)
  "创建 org-inc topic，标题为 TITLE。"
  (unless (org-id-get)
    (org-id-get-create))
  (org-srs-item-new 'topic)
  (message "✓ 已创建 org-inc topic: %s" title))

(defun doc-capture-org-inc--insert-link (org-file page-num)
  "插入返回原文档的链接。"
  (let ((file-name (file-name-nondirectory org-file)))
    (insert (format "[[elisp:(doc-capture-open \"%s\" %s)][%s - 第%s页]]\n\n"
                    org-file page-num file-name page-num))))

;;; 处理器函数

(defun doc-capture-org-inc-topic-handler (org-file page-num selected-text _heading)
  "基础版本：在捕获位置创建 org-inc topic 格式的卡片。"
  (when (and selected-text (> (length selected-text) 0))
    (condition-case err
        (with-current-buffer (find-file-noselect org-file)
          (doc-capture-org-inc--ensure-notes-section)
          
          (let* ((file-base (file-name-sans-extension
                            (file-name-nondirectory org-file)))
                 (title (format "%s - 第%s页" file-base page-num)))
            
            ;; 插入标题和内容
            (insert title "\n" selected-text "\n\n")
            
            ;; 创建 topic
            (save-excursion
              (forward-line -2)
              (end-of-line)
              (doc-capture-org-inc--create-topic title))
            
            ;; 添加链接
            (doc-capture-org-inc--insert-link org-file page-num)
            (save-buffer)))
      (error (message "❌ org-inc 集成失败: %s" err)))))

(defun doc-capture-org-inc-smart-handler (org-file page-num selected-text _heading)
  "智能版本：根据文档类型和内容长度自动调整。"
  (when (and selected-text (> (length selected-text) 0))
    (condition-case err
        (with-current-buffer (find-file-noselect org-file)
          (doc-capture-org-inc--ensure-notes-section)
          
          (let* ((file-ext (file-name-extension org-file))
                 (file-base (file-name-sans-extension
                            (file-name-nondirectory org-file)))
                 ;; 文档分类
                 (category (cond ((string= file-ext "pdf") "PDF")
                               ((member file-ext '("epub" "mobi" "azw" "azw3")) "电子书")
                               ((string= file-ext "djvu") "DJVU")
                               (t "文档")))
                 ;; 优先级
                 (priority (cond ((string= file-ext "pdf") 0.9)
                               ((member file-ext '("epub" "mobi")) 0.7)
                               (t 0.5)))
                 (title (format "[%s] %s - 第%s页" category file-base page-num))
                 (content-length (length selected-text)))
            
            ;; 插入标题
            (insert title "\n")
            
            ;; 根据长度调整内容格式
            (if (> content-length 200)
                (insert (format "内容摘要 (%d 字符):\n%s...\n\n完整内容请参见原文链接。\n\n"
                              content-length
                              (substring selected-text 0 (min 100 content-length))))
              (insert selected-text "\n\n"))
            
            ;; 创建 topic 并设置优先级
            (save-excursion
              (forward-line -3)
              (end-of-line)
              (doc-capture-org-inc--create-topic title)
              (org-inc-priority-set priority)
              (message "✓ 智能 topic 创建成功: %s (优先级: %.1f)" title priority))
            
            ;; 添加链接
            (doc-capture-org-inc--insert-link org-file page-num)
            (save-buffer)))
      (error (message "❌ 智能 org-inc 处理失败: %s" err)))))

(defun doc-capture-org-inc-conditional-handler (org-file page-num selected-text heading)
  "条件版本：只对 PDF 和电子书使用 org-inc。"
  (let ((file-ext (file-name-extension org-file)))
    (if (member file-ext '("pdf" "epub" "mobi" "azw" "azw3"))
        (doc-capture-org-inc-topic-handler org-file page-num selected-text heading)
      (message "跳过 org-inc 处理，文档类型: %s" file-ext))))

;;; 自动配置

(defun doc-capture-org-inc-auto-configure ()
  "根据 `doc-capture-org-inc-handler-type' 自动配置集成。"
  (when (boundp 'doc-capture-post-process-hook)
    ;; 清除旧的 handler
    (dolist (handler '(doc-capture-org-inc-topic-handler
                      doc-capture-org-inc-smart-handler
                      doc-capture-org-inc-conditional-handler))
      (remove-hook 'doc-capture-post-process-hook handler))
    
    ;; 添加新的 handler
    (when (and doc-capture-enable-hook
               doc-capture-org-inc-handler-type)
      (let ((handler (pcase doc-capture-org-inc-handler-type
                      ('topic 'doc-capture-org-inc-topic-handler)
                      ('smart 'doc-capture-org-inc-smart-handler)
                      ('conditional 'doc-capture-org-inc-conditional-handler))))
        (when handler
          (add-hook 'doc-capture-post-process-hook handler)
          (message "✓ doc-capture-org-inc-integration: 已启用 %s 处理器" 
                   doc-capture-org-inc-handler-type))))))

;; 自动配置触发
(with-eval-after-load 'doc-capture
  (doc-capture-org-inc-auto-configure))

(when (boundp 'doc-capture-post-process-hook)
  (doc-capture-org-inc-auto-configure))

(provide 'doc-capture-org-inc-integration)
;;; doc-capture-org-inc-integration.el ends here
