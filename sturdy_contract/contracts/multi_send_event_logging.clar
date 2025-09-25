;; Multi-Send Smart Contract with Event Logging
;; Allows sending STX to multiple recipients with comprehensive event tracking

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-TRANSFER-FAILED (err u103))
(define-constant ERR-EMPTY-RECIPIENT-LIST (err u104))
(define-constant ERR-INVALID-RECIPIENT (err u105))

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; Event logging for individual transfers
(define-map transfer-logs
  { batch-id: uint, transfer-index: uint }
  {
    sender: principal,
    recipient: principal,
    amount: uint,
    block-height: uint,
    timestamp: uint
  }
)

;; Batch tracking
(define-data-var batch-counter uint u0)

;; Events for off-chain tracking
(define-private (log-transfer-event 
  (batch-id uint)
  (transfer-index uint)
  (sender principal)
  (recipient principal) 
  (amount uint))
  (begin
    ;; Store detailed transfer log
    (map-set transfer-logs
      { batch-id: batch-id, transfer-index: transfer-index }
      {
        sender: sender,
        recipient: recipient,
        amount: amount,
        block-height: block-height,
        timestamp: (unwrap-panic (get-block-info? time block-height))
      }
    )
    ;; Print event for off-chain monitoring
    (print {
      event: "transfer-completed",
      batch-id: batch-id,
      transfer-index: transfer-index,
      sender: sender,
      recipient: recipient,
      amount: amount,
      block-height: block-height
    })
  )
)

;; Log batch completion event
(define-private (log-batch-event 
  (batch-id uint)
  (sender principal)
  (total-recipients uint)
  (total-amount uint)
  (successful-transfers uint))
  (print {
    event: "batch-completed",
    batch-id: batch-id,
    sender: sender,
    total-recipients: total-recipients,
    total-amount: total-amount,
    successful-transfers: successful-transfers,
    block-height: block-height
  })
)

;; Single transfer function with event logging
(define-private (execute-transfer
  (batch-id uint)
  (transfer-index uint)
  (recipient principal)
  (amount uint))
  (let ((sender tx-sender))
    (match (stx-transfer? amount sender recipient)
      success (begin
        (log-transfer-event batch-id transfer-index sender recipient amount)
        (ok { recipient: recipient, amount: amount, success: true })
      )
      error (begin
        (print {
          event: "transfer-failed",
          batch-id: batch-id,
          transfer-index: transfer-index,
          sender: sender,
          recipient: recipient,
          amount: amount,
          error: error
        })
        (ok { recipient: recipient, amount: amount, success: false })
      )
    )
  )
)

;; Process a list of transfers
(define-private (process-transfers
  (batch-id uint)
  (transfers (list 100 { recipient: principal, amount: uint })))
  (let ((results (map execute-single-transfer 
                      (enumerate transfers))))
    results
  )
)

;; Helper function to execute single transfer with index
(define-private (execute-single-transfer
  (transfer-data { index: uint, item: { recipient: principal, amount: uint } }))
  (let ((batch-id (var-get batch-counter))
        (transfer-index (get index transfer-data))
        (recipient (get recipient (get item transfer-data)))
        (amount (get amount (get item transfer-data))))
    (execute-transfer batch-id transfer-index recipient amount)
  )
)

;; Helper function to enumerate list items with indices
(define-private (enumerate 
  (items (list 100 { recipient: principal, amount: uint })))
  (map add-index items (list 
    u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19
    u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32 u33 u34 u35 u36 u37 u38 u39
    u40 u41 u42 u43 u44 u45 u46 u47 u48 u49 u50 u51 u52 u53 u54 u55 u56 u57 u58 u59
    u60 u61 u62 u63 u64 u65 u66 u67 u68 u69 u70 u71 u72 u73 u74 u75 u76 u77 u78 u79
    u80 u81 u82 u83 u84 u85 u86 u87 u88 u89 u90 u91 u92 u93 u94 u95 u96 u97 u98 u99
  ))
)

;; Helper to add index to item
(define-private (add-index 
  (item { recipient: principal, amount: uint })
  (index uint))
  { index: index, item: item }
)

;; Calculate total amount needed
(define-private (sum-amounts 
  (transfers (list 100 { recipient: principal, amount: uint })))
  (fold + (map get-amount transfers) u0)
)

(define-private (get-amount (transfer { recipient: principal, amount: uint }))
  (get amount transfer)
)

;; Validate transfer list
(define-private (validate-transfers
  (transfers (list 100 { recipient: principal, amount: uint })))
  (let ((total-amount (sum-amounts transfers))
        (sender-balance (stx-get-balance tx-sender)))
    (and
      (> (len transfers) u0) ;; Not empty
      (> total-amount u0)    ;; Total amount > 0
      (>= sender-balance total-amount) ;; Sufficient balance
    )
  )
)

;; Main multi-send function
(define-public (multi-send 
  (transfers (list 100 { recipient: principal, amount: uint })))
  (let ((batch-id (+ (var-get batch-counter) u1))
        (sender tx-sender)
        (total-recipients (len transfers))
        (total-amount (sum-amounts transfers)))
    
    ;; Validate inputs
    (asserts! (> total-recipients u0) ERR-EMPTY-RECIPIENT-LIST)
    (asserts! (validate-transfers transfers) ERR-INSUFFICIENT-BALANCE)
    
    ;; Update batch counter
    (var-set batch-counter batch-id)
    
    ;; Log batch start
    (print {
      event: "batch-started",
      batch-id: batch-id,
      sender: sender,
      total-recipients: total-recipients,
      total-amount: total-amount
    })
    
    ;; Process all transfers
    (let ((results (map execute-indexed-transfer (enumerate transfers))))
      (let ((successful-count (len (filter is-successful results))))
        
        ;; Log batch completion
        (log-batch-event batch-id sender total-recipients total-amount successful-count)
        
        (ok {
          batch-id: batch-id,
          total-recipients: total-recipients,
          successful-transfers: successful-count,
          total-amount: total-amount,
          results: results
        })
      )
    )
  )
)

;; Execute transfer with proper indexing
(define-private (execute-indexed-transfer
  (transfer-data { index: uint, item: { recipient: principal, amount: uint } }))
  (let ((batch-id (var-get batch-counter))
        (transfer-index (get index transfer-data))
        (recipient (get recipient (get item transfer-data)))
        (amount (get amount (get item transfer-data))))
    (execute-transfer batch-id transfer-index recipient amount)
  )
)

;; Helper to check if transfer was successful
(define-private (is-successful (result (response { recipient: principal, amount: uint, success: bool } uint)))
  (match result
    success-data (get success success-data)
    error false
  )
)

;; Read-only functions for querying transfer history

;; Get transfer log by batch and index
(define-read-only (get-transfer-log (batch-id uint) (transfer-index uint))
  (map-get? transfer-logs { batch-id: batch-id, transfer-index: transfer-index })
)

;; Get current batch counter
(define-read-only (get-current-batch-id)
  (var-get batch-counter)
)

;; Get contract owner
(define-read-only (get-contract-owner)
  (var-get contract-owner)
)

;; Emergency functions (owner only)
(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (var-set contract-owner new-owner)
    (print { event: "owner-changed", old-owner: tx-sender, new-owner: new-owner })
    (ok true)
  )
)