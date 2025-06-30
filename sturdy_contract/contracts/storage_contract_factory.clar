;; StorageContractFactory Smart Contract
;; Purpose: Deploy custom storage agreements with automated payments and renewals

;; Define constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_DURATION (err u101))
(define-constant ERR_INVALID_PRICE (err u102))
(define-constant ERR_CONTRACT_NOT_FOUND (err u103))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u104))
(define-constant ERR_CONTRACT_EXPIRED (err u105))
(define-constant ERR_CONTRACT_ALREADY_EXISTS (err u106))
(define-constant ERR_RENEWAL_TOO_EARLY (err u107))

;; Define data variables
(define-data-var contract-counter uint u0)
(define-data-var factory-fee uint u1000000) ;; 1 STX in microSTX

;; Define storage contract structure
(define-map storage-contracts
  { contract-id: uint }
  {
    client: principal,
    provider: principal,
    file-hash: (optional (buff 32)),
    storage-type: (string-ascii 20), ;; "per-file" or "per-user"
    duration-blocks: uint,
    price-per-block: uint,
    total-price: uint,
    created-at: uint,
    expires-at: uint,
    is-active: bool,
    auto-renew: bool,
    payments-made: uint
  }
)

;; Map to track user's contracts
(define-map user-contracts
  { user: principal }
  { contract-ids: (list 100 uint) }
)

;; Map to track provider's contracts
(define-map provider-contracts
  { provider: principal }
  { contract-ids: (list 100 uint) }
)

;; Payment escrow for contracts
(define-map contract-escrow
  { contract-id: uint }
  { amount: uint, last-payment: uint }
)

;; Read-only functions

;; Get contract details
(define-read-only (get-contract (contract-id uint))
  (map-get? storage-contracts { contract-id: contract-id })
)

;; Get user's contracts
(define-read-only (get-user-contracts (user principal))
  (default-to 
    { contract-ids: (list) }
    (map-get? user-contracts { user: user })
  )
)

;; Get provider's contracts
(define-read-only (get-provider-contracts (provider principal))
  (default-to 
    { contract-ids: (list) }
    (map-get? provider-contracts { provider: provider })
  )
)

;; Check if contract is active and not expired
(define-read-only (is-contract-valid (contract-id uint))
  (match (get-contract contract-id)
    contract-data 
    (and 
      (get is-active contract-data)
      (> (get expires-at contract-data) block-height)
    )
    false
  )
)

;; Get contract escrow balance
(define-read-only (get-contract-escrow (contract-id uint))
  (map-get? contract-escrow { contract-id: contract-id })
)

;; Calculate renewal price
(define-read-only (calculate-renewal-price (contract-id uint))
  (match (get-contract contract-id)
    contract-data 
    (ok (get total-price contract-data))
    ERR_CONTRACT_NOT_FOUND
  )
)

;; Private functions

;; Add contract to user's list
(define-private (add-contract-to-user (user principal) (contract-id uint))
  (let (
    (current-contracts (get contract-ids (get-user-contracts user)))
    (new-contracts (unwrap! (as-max-len? (append current-contracts contract-id) u100) ERR_UNAUTHORIZED))
  )
    (ok (map-set user-contracts
      { user: user }
      { contract-ids: new-contracts }
    ))
  )
)

;; Add contract to provider's list
(define-private (add-contract-to-provider (provider principal) (contract-id uint))
  (let (
    (current-contracts (get contract-ids (get-provider-contracts provider)))
    (new-contracts (unwrap! (as-max-len? (append current-contracts contract-id) u100) ERR_UNAUTHORIZED))
  )
    (ok (map-set provider-contracts
      { provider: provider }
      { contract-ids: new-contracts }
    ))
  )
)

;; Public functions

;; Create a new storage contract
(define-public (create-storage-contract 
  (provider principal)
  (file-hash (optional (buff 32)))
  (storage-type (string-ascii 20))
  (duration-blocks uint)
  (price-per-block uint)
  (auto-renew bool)
)
  (let (
    (contract-id (+ (var-get contract-counter) u1))
    (total-price (* duration-blocks price-per-block))
    (expires-at (+ block-height duration-blocks))
    (factory-fee-amount (var-get factory-fee))
  )
    ;; Validate inputs
    (asserts! (> duration-blocks u0) ERR_INVALID_DURATION)
    (asserts! (> price-per-block u0) ERR_INVALID_PRICE)
    (asserts! (or (is-eq storage-type "per-file") (is-eq storage-type "per-user")) ERR_INVALID_DURATION)
    
    ;; Transfer factory fee to contract owner
    (try! (stx-transfer? factory-fee-amount tx-sender CONTRACT_OWNER))
    
    ;; Transfer total payment to escrow
    (try! (stx-transfer? total-price tx-sender (as-contract tx-sender)))
    
    ;; Create the contract
    (map-set storage-contracts
      { contract-id: contract-id }
      {
        client: tx-sender,
        provider: provider,
        file-hash: file-hash,
        storage-type: storage-type,
        duration-blocks: duration-blocks,
        price-per-block: price-per-block,
        total-price: total-price,
        created-at: block-height,
        expires-at: expires-at,
        is-active: true,
        auto-renew: auto-renew,
        payments-made: u1
      }
    )
    
    ;; Set up escrow
    (map-set contract-escrow
      { contract-id: contract-id }
      { amount: total-price, last-payment: block-height }
    )
    
    ;; Add to user and provider lists
    (try! (add-contract-to-user tx-sender contract-id))
    (try! (add-contract-to-provider provider contract-id))
    
    ;; Update counter
    (var-set contract-counter contract-id)
    
    (ok contract-id)
  )
)

;; Release payment to provider (called when storage service is confirmed)
(define-public (release-payment (contract-id uint))
  (let (
    (contract-data (unwrap! (get-contract contract-id) ERR_CONTRACT_NOT_FOUND))
    (escrow-data (unwrap! (get-contract-escrow contract-id) ERR_CONTRACT_NOT_FOUND))
  )
    ;; Only client or provider can release payment
    (asserts! (or (is-eq tx-sender (get client contract-data)) 
                  (is-eq tx-sender (get provider contract-data))) ERR_UNAUTHORIZED)
    
    ;; Contract must be valid
    (asserts! (is-contract-valid contract-id) ERR_CONTRACT_EXPIRED)
    
    ;; Transfer payment to provider
    (try! (as-contract (stx-transfer? (get amount escrow-data) tx-sender (get provider contract-data))))
    
    ;; Clear escrow
    (map-delete contract-escrow { contract-id: contract-id })
    
    (ok true)
  )
)

;; Renew contract
(define-public (renew-contract (contract-id uint))
  (let (
    (contract-data (unwrap! (get-contract contract-id) ERR_CONTRACT_NOT_FOUND))
    (blocks-until-expiry (- (get expires-at contract-data) block-height))
  )
    ;; Only client can renew
    (asserts! (is-eq tx-sender (get client contract-data)) ERR_UNAUTHORIZED)
    
    ;; Contract must be active
    (asserts! (get is-active contract-data) ERR_CONTRACT_EXPIRED)
    
    ;; Can only renew within 1000 blocks of expiry
    (asserts! (<= blocks-until-expiry u1000) ERR_RENEWAL_TOO_EARLY)
    
    ;; Transfer renewal payment
    (try! (stx-transfer? (get total-price contract-data) tx-sender (as-contract tx-sender)))
    
    ;; Update contract
    (map-set storage-contracts
      { contract-id: contract-id }
      (merge contract-data {
        expires-at: (+ (get expires-at contract-data) (get duration-blocks contract-data)),
        payments-made: (+ (get payments-made contract-data) u1)
      })
    )
    
    ;; Update escrow
    (map-set contract-escrow
      { contract-id: contract-id }
      { amount: (get total-price contract-data), last-payment: block-height }
    )
    
    (ok true)
  )
)

;; Auto-renew contract (can be called by anyone to trigger auto-renewal)
(define-public (auto-renew-contract (contract-id uint))
  (let (
    (contract-data (unwrap! (get-contract contract-id) ERR_CONTRACT_NOT_FOUND))
  )
    ;; Contract must have auto-renew enabled
    (asserts! (get auto-renew contract-data) ERR_UNAUTHORIZED)
    
    ;; Contract must be expired or about to expire (within 100 blocks)
    (asserts! (<= (- (get expires-at contract-data) block-height) u100) ERR_RENEWAL_TOO_EARLY)
    
    ;; Contract must be active
    (asserts! (get is-active contract-data) ERR_CONTRACT_EXPIRED)
    
    ;; For auto-renewal, payment should come from client's balance (simplified here)
    ;; In a full implementation, you'd want a more sophisticated payment mechanism
    
    ;; Update contract expiry
    (map-set storage-contracts
      { contract-id: contract-id }
      (merge contract-data {
        expires-at: (+ (get expires-at contract-data) (get duration-blocks contract-data)),
        payments-made: (+ (get payments-made contract-data) u1)
      })
    )
    
    (ok true)
  )
)

;; Cancel contract (before expiry)
(define-public (cancel-contract (contract-id uint))
  (let (
    (contract-data (unwrap! (get-contract contract-id) ERR_CONTRACT_NOT_FOUND))
    (escrow-data (get-contract-escrow contract-id))
  )
    ;; Only client can cancel
    (asserts! (is-eq tx-sender (get client contract-data)) ERR_UNAUTHORIZED)
    
    ;; Contract must be active
    (asserts! (get is-active contract-data) ERR_CONTRACT_EXPIRED)
    
    ;; Deactivate contract
    (map-set storage-contracts
      { contract-id: contract-id }
      (merge contract-data { is-active: false })
    )
    
    ;; Refund remaining balance (simplified calculation)
    (match escrow-data
      escrow 
      (let (
        (blocks-remaining (- (get expires-at contract-data) block-height))
        (refund-amount (/ (* (get amount escrow) blocks-remaining) (get duration-blocks contract-data)))
      )
        (try! (as-contract (stx-transfer? refund-amount tx-sender (get client contract-data))))
        (try! (as-contract (stx-transfer? (- (get amount escrow) refund-amount) tx-sender (get provider contract-data))))
        (map-delete contract-escrow { contract-id: contract-id })
      )
      true ;; No escrow to handle
    )
    
    (ok true)
  )
)

;; Admin function to update factory fee
(define-public (set-factory-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set factory-fee new-fee)
    (ok true)
  )
)

;; Emergency function to pause a contract (admin only)
(define-public (emergency-pause-contract (contract-id uint))
  (let (
    (contract-data (unwrap! (get-contract contract-id) ERR_CONTRACT_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    (map-set storage-contracts
      { contract-id: contract-id }
      (merge contract-data { is-active: false })
    )
    
    (ok true)
  )
)