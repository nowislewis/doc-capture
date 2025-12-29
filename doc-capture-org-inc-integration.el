;;; doc-capture-org-inc-integration.el --- org-inc integration for doc-capture -*- lexical-binding: t; -*-

;; Copyright (C) 2024
;; Package-Requires: ((emacs "24.4") (doc-capture "1.0") (org-inc "0.1"))
;; Keywords: org, capture, srs

;;; Commentary:
;;
;; 为 doc-capture 捕获的内容自动创建 org-inc SRS 卡片。
;; 通过 doc-capture-before-finalize-hook 实现，仅对 doc-capture 模板生效。
;; 用法：(require 'doc-capture-org-inc-integration)

;;; Code:

(require 'doc-capture)
(require 'org-inc)

(defgroup doc-capture-org-inc nil
  "org-inc integration for doc-capture."
  :group 'doc-capture)

(defcustom doc-capture-org-inc-enable t
  "是否为 doc-capture 自动创建 org-inc SRS 卡片。"
  :type 'boolean
  :group 'doc-capture-org-inc)

(defun doc-capture-org-inc-create-topic ()
  "为当前 heading 创建 org-inc topic。"
  (when doc-capture-org-inc-enable
    (condition-case err
        (save-excursion
          (org-back-to-heading t)
          (unless (org-id-get)
            (org-id-get-create))
          (org-srs-item-new 'topic)
          (message "✓ 已创建 org-inc topic"))
      (error
       (message "org-inc 集成失败: %s" (error-message-string err))))))

;; 自动启用
(add-hook 'doc-capture-before-finalize-hook #'doc-capture-org-inc-create-topic)

(provide 'doc-capture-org-inc-integration)
;;; doc-capture-org-inc-integration.el ends here
