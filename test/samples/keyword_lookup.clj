;; cloudflare-secret

(defn read-secret [env]
  (:TELEGRAM_WEBHOOK_SECRET env))

(defn test []
  (read-secret {:TELEGRAM_WEBHOOK_SECRET "cloudflare-secret"}))
