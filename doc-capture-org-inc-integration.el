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
- topic: 基础版本，为每个捕获创建 org-inc topic
- conditional: 条件版本，只对 PDF/电子书使用 org-inc
- nil: 禁用 org-inc 集成"
  :type '(choice (const :tag "基础版本" topic)
                 (const :tag "条件版本" conditional)
                 (const :tag "禁用" nil))
  :group 'doc-capture)

;;; 处理器函数

(defun doc-capture-org-inc-topic-handler (org-file page-num selected-text heading)
  "基础版本：在捕获位置创建 org-inc topic 格式的卡片。
此函数在 doc-capture 已经插入内容后被调用，会在当前 buffer 中添加 org-inc 属性。"
  (when (and selected-text (> (length selected-text) 0))
    (condition-case err
        (save-excursion
          ;; 找到刚刚插入的 heading (应该是最后一个 ** Page X)
          (goto-char (point-max))
          (when (re-search-backward (format "^\\*\\* Page %s$" page-num) nil t)
            ;; 移动到 heading 的末尾
            (end-of-line)
            
            ;; 确保有 ID
            (unless (org-id-get)
              (org-id-get-create))
            
            ;; 创建 topic
            (org-srs-item-new 'topic)
            
            (let* ((file-base (file-name-sans-extension
                              (file-name-nondirectory org-file)))
                   (title (format "%s - 第%s页" file-base page-num)))
              (message "✓ 已创建 org-inc topic: %s" title))
            
            ;; 保存 buffer
            (save-buffer)))
      (error (message "❌ org-inc 集成失败: %s" (error-message-string err))))))

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
                      doc-capture-org-inc-conditional-handler))
      (remove-hook 'doc-capture-post-process-hook handler))
    
    ;; 添加新的 handler
    (when (and doc-capture-enable-hook
               doc-capture-org-inc-handler-type)
      (let ((handler (pcase doc-capture-org-inc-handler-type
                      ('topic 'doc-capture-org-inc-topic-handler)
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
