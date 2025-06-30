;; Enhanced Pod or Storage with Payment Smart Contract
;; Advanced features for paid storage management

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-INSUFFICIENT-PAYMENT (err u100))
(define-constant ERR-TRANSFER-FAILED (err u101))
(define-constant ERR-UNAUTHORIZED (err u102))
(define-constant ERR-ALREADY-VOTED (err u103))
(define-constant ERR-PROPOSAL-EXPIRED (err u104))
(define-constant ERR-INVALID-DISCOUNT (err u105))
(define-constant ERR-BLACKLISTED (err u106))
(define-constant ERR-UPDATE-TOO-SOON (err u107))
(define-constant ERR-INVALID-VALUE (err u108))
(define-constant ERR-REFUND-FAILED (err u109))

;; Data Variables
(define-data-var stored-value (string-utf8 256) u"")
(define-data-var update-fee uint u1000000) ;; 1 STX in microSTX
(define-data-var total-fees-collected uint u0)
(define-data-var update-count uint u0)
(define-data-var last-update-block uint block-height)
(define-data-var last-updater principal tx-sender)
(define-data-var min-blocks-between-updates uint u0) ;; Rate limiting
(define-data-var max-value-length uint u256)
(define-data-var proposal-count uint u0)
(define-data-var revenue-share-percentage uint u10) ;; 10% to frequent updaters

;; Data Maps
(define-map user-updates principal uint)
(define-map user-total-paid principal uint)
(define-map user-last-update principal uint)
(define-map user-discounts principal uint) ;; Percentage discount (0-100)
(define-map blacklisted-users principal bool)
(define-map whitelisted-users principal bool) ;; Free updates
(define-map update-history uint {
    updater: principal,
    value: (string-utf8 256),
    fee-paid: uint,
    block: uint,
    timestamp: uint
})

;; Governance proposals
(define-map proposals uint {
    proposer: principal,
    new-value: (string-utf8 256),
    votes-for: uint,
    votes-against: uint,
    expires-at: uint,
    executed: bool
})
(define-map user-votes {proposal-id: uint, voter: principal} bool)

;; Subscription system
(define-map subscriptions principal {
    expires-at: uint,
    updates-remaining: uint,
    tier: (string-ascii 10)
})

;; Read-only functions

;; Get the current stored value
(define-read-only (get-value)
    (ok (var-get stored-value))
)

;; Get detailed information about the current state
(define-read-only (get-state-info)
    (ok {
        value: (var-get stored-value),
        update-fee: (var-get update-fee),
        total-fees: (var-get total-fees-collected),
        update-count: (var-get update-count),
        last-update-block: (var-get last-update-block),
        last-updater: (var-get last-updater)
    })
)

;; Get user statistics
(define-read-only (get-user-stats (user principal))
    (ok {
        update-count: (default-to u0 (map-get? user-updates user)),
        total-paid: (default-to u0 (map-get? user-total-paid user)),
        last-update: (default-to u0 (map-get? user-last-update user)),
        discount: (default-to u0 (map-get? user-discounts user)),
        is-blacklisted: (default-to false (map-get? blacklisted-users user)),
        is-whitelisted: (default-to false (map-get? whitelisted-users user)),
        subscription: (map-get? subscriptions user)
    })
)

;; Get update history by index
(define-read-only (get-update-history (index uint))
    (ok (map-get? update-history index))
)

;; Calculate actual fee for user (with discounts)
(define-read-only (get-user-fee (user principal))
    (let
        (
            (base-fee (var-get update-fee))
            (discount (default-to u0 (map-get? user-discounts user)))
            (is-whitelisted (default-to false (map-get? whitelisted-users user)))
            (subscription (map-get? subscriptions user))
        )
        ;; Whitelisted users pay nothing
        (if is-whitelisted
            (ok u0)
            ;; Check subscription
            (match subscription
                sub-data
                (if (and (> (get expires-at sub-data) block-height) 
                         (> (get updates-remaining sub-data) u0))
                    (ok u0)
                    ;; Apply discount if subscription expired or no updates left
                    (ok (- base-fee (/ (* base-fee discount) u100))))
                ;; No subscription, apply discount
                (ok (- base-fee (/ (* base-fee discount) u100)))
            )
        )
    )
)

;; Get proposal details
(define-read-only (get-proposal (proposal-id uint))
    (ok (map-get? proposals proposal-id))
)

;; Public functions

;; Update the stored value with payment
(define-public (update-value (new-value (string-utf8 256)))
    (let
        (
            (sender tx-sender)
            (user-fee (unwrap! (get-user-fee sender) ERR-TRANSFER-FAILED))
            (last-user-update (default-to u0 (map-get? user-last-update sender)))
            (update-gap (- block-height last-user-update))
            (is-blacklisted (default-to false (map-get? blacklisted-users sender)))
            (value-length (len new-value))
        )
        ;; Check if user is blacklisted
        (asserts! (not is-blacklisted) ERR-BLACKLISTED)
        
        ;; Check rate limiting
        (asserts! (or (is-eq last-user-update u0) 
                     (>= update-gap (var-get min-blocks-between-updates))) 
                  ERR-UPDATE-TOO-SOON)
        
        ;; Check value length
        (asserts! (<= value-length (var-get max-value-length)) ERR-INVALID-VALUE)
        
        ;; Process payment if required
        (if (> user-fee u0)
            (try! (stx-transfer? user-fee sender (as-contract tx-sender)))
            true
        )
        
        ;; Check and update subscription if applicable
        (match (map-get? subscriptions sender)
            sub-data
            (if (and (> (get expires-at sub-data) block-height) 
                     (> (get updates-remaining sub-data) u0))
                (map-set subscriptions sender 
                    (merge sub-data {updates-remaining: (- (get updates-remaining sub-data) u1)}))
                true)
            true
        )
        
        ;; Update the stored value
        (var-set stored-value new-value)
        
        ;; Update statistics
        (var-set total-fees-collected (+ (var-get total-fees-collected) user-fee))
        (var-set update-count (+ (var-get update-count) u1))
        (var-set last-update-block block-height)
        (var-set last-updater sender)
        
        ;; Update user statistics
        (map-set user-updates sender 
            (+ (default-to u0 (map-get? user-updates sender)) u1))
        (map-set user-total-paid sender 
            (+ (default-to u0 (map-get? user-total-paid sender)) user-fee))
        (map-set user-last-update sender block-height)
        
        ;; Store in history
        (map-set update-history (var-get update-count) {
            updater: sender,
            value: new-value,
            fee-paid: user-fee,
            block: block-height,
            timestamp: block-height ;; Using block-height as timestamp proxy
        })
        
        ;; Award loyalty discount for frequent users
        (if (>= (default-to u0 (map-get? user-updates sender)) u10)
            (map-set user-discounts sender u5) ;; 5% discount after 10 updates
            true
        )
        
        (ok {success: true, fee-paid: user-fee})
    )
)

;; Batch update multiple values (requires multiple payments)
(define-public (batch-update (values (list 10 (string-utf8 256))))
    (fold batch-update-iter values (ok {updates: u0, total-fees: u0}))
)

(define-private (batch-update-iter (value (string-utf8 256)) (result (response {updates: uint, total-fees: uint} uint)))
    (match result
        success-data
        (match (update-value value)
            update-result
            (ok {
                updates: (+ (get updates success-data) u1),
                total-fees: (+ (get total-fees success-data) (get fee-paid update-result))
            })
            error-code (err error-code)
        )
        error-code (err error-code)
    )
)

;; Create a governance proposal
(define-public (create-proposal (new-value (string-utf8 256)))
    (let
        (
            (proposal-id (+ (var-get proposal-count) u1))
            (proposer tx-sender)
        )
        ;; Require proposer to have made at least 5 updates
        (asserts! (>= (default-to u0 (map-get? user-updates proposer)) u5) ERR-UNAUTHORIZED)
        
        (map-set proposals proposal-id {
            proposer: proposer,
            new-value: new-value,
            votes-for: u1, ;; Proposer automatically votes for
            votes-against: u0,
            expires-at: (+ block-height u1440), ;; ~24 hours
            executed: false
        })
        
        (map-set user-votes {proposal-id: proposal-id, voter: proposer} true)
        (var-set proposal-count proposal-id)
        
        (ok proposal-id)
    )
)

;; Vote on a proposal
(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
    (let
        (
            (proposal (unwrap! (map-get? proposals proposal-id) ERR-UNAUTHORIZED))
            (voter tx-sender)
            (has-voted (default-to false (map-get? user-votes {proposal-id: proposal-id, voter: voter})))
        )
        ;; Check if already voted
        (asserts! (not has-voted) ERR-ALREADY-VOTED)
        
        ;; Check if proposal expired
        (asserts! (<= block-height (get expires-at proposal)) ERR-PROPOSAL-EXPIRED)
        
        ;; Check if voter has made at least 1 update
        (asserts! (> (default-to u0 (map-get? user-updates voter)) u0) ERR-UNAUTHORIZED)
        
        ;; Record vote
        (map-set user-votes {proposal-id: proposal-id, voter: voter} true)
        
        ;; Update vote counts
        (if vote-for
            (map-set proposals proposal-id 
                (merge proposal {votes-for: (+ (get votes-for proposal) u1)}))
            (map-set proposals proposal-id 
                (merge proposal {votes-against: (+ (get votes-against proposal) u1)}))
        )
        
        (ok true)
    )
)

;; Execute a proposal if it has enough votes
(define-public (execute-proposal (proposal-id uint))
    (let
        (
            (proposal (unwrap! (map-get? proposals proposal-id) ERR-UNAUTHORIZED))
            (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
        )
        ;; Check if already executed
        (asserts! (not (get executed proposal)) ERR-UNAUTHORIZED)
        
        ;; Check if proposal expired
        (asserts! (<= block-height (get expires-at proposal)) ERR-PROPOSAL-EXPIRED)
        
        ;; Require at least 3 votes and majority for
        (asserts! (and (>= total-votes u3) 
                      (> (get votes-for proposal) (get votes-against proposal))) 
                  ERR-UNAUTHORIZED)
        
        ;; Execute the proposal
        (var-set stored-value (get new-value proposal))
        (map-set proposals proposal-id (merge proposal {executed: true}))
        
        ;; Reward proposer with 20% discount
        (map-set user-discounts (get proposer proposal) u20)
        
        (ok true)
    )
)

;; Purchase a subscription
(define-public (purchase-subscription (tier (string-ascii 10)))
    (let
        (
            (subscriber tx-sender)
            (subscription-fee (if (is-eq tier "basic") u5000000 
                                (if (is-eq tier "premium") u10000000 u20000000))) ;; 5, 10, or 20 STX
            (updates-allowed (if (is-eq tier "basic") u10 
                               (if (is-eq tier "premium") u25 u100)))
            (duration (if (is-eq tier "basic") u4320 
                        (if (is-eq tier "premium") u8640 u17280))) ;; ~30, 60, or 120 days
        )
        ;; Transfer subscription fee
        (try! (stx-transfer? subscription-fee subscriber (as-contract tx-sender)))
        
        ;; Set subscription
        (map-set subscriptions subscriber {
            expires-at: (+ block-height duration),
            updates-remaining: updates-allowed,
            tier: tier
        })
        
        (ok {tier: tier, expires-at: (+ block-height duration), updates: updates-allowed})
    )
)

;; Admin functions

;; Set update fee
(define-public (set-update-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        (var-set update-fee new-fee)
        (ok true)
    )
)

;; Set rate limiting
(define-public (set-rate-limit (blocks uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        (var-set min-blocks-between-updates blocks)
        (ok true)
    )
)

;; Blacklist a user
(define-public (blacklist-user (user principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        (map-set blacklisted-users user true)
        (ok true)
    )
)

;; Remove from blacklist
(define-public (unblacklist-user (user principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        (map-delete blacklisted-users user)
        (ok true)
    )
)

;; Whitelist a user (free updates)
(define-public (whitelist-user (user principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        (map-set whitelisted-users user true)
        (ok true)
    )
)

;; Set user discount
(define-public (set-user-discount (user principal) (discount uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        (asserts! (<= discount u100) ERR-INVALID-DISCOUNT)
        (map-set user-discounts user discount)
        (ok true)
    )
)

;; Withdraw fees with revenue sharing
(define-public (withdraw-fees (recipient principal))
    (let
        (
            (balance (var-get total-fees-collected))
            (top-updater (var-get last-updater)) ;; Simplified - could track actual top updater
            (revenue-share (/ (* balance (var-get revenue-share-percentage)) u100))
            (owner-share (- balance revenue-share))
        )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        (asserts! (> balance u0) ERR-INSUFFICIENT-PAYMENT)
        
        ;; Transfer revenue share to top contributor
        (if (> revenue-share u0)
            (try! (as-contract (stx-transfer? revenue-share tx-sender top-updater)))
            true
        )
        
        ;; Transfer remaining to recipient
        (try! (as-contract (stx-transfer? owner-share tx-sender recipient)))
        
        ;; Reset collected fees
        (var-set total-fees-collected u0)
        
        (ok {total: balance, owner-share: owner-share, revenue-share: revenue-share})
    )
)

;; Emergency refund function
(define-public (emergency-refund (user principal) (amount uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        (asserts! (<= amount (var-get total-fees-collected)) ERR-INSUFFICIENT-PAYMENT)
        
        (try! (as-contract (stx-transfer? amount tx-sender user)))
        (var-set total-fees-collected (- (var-get total-fees-collected) amount))
        
        (ok true)
    )
)

;; Set maximum value length
(define-public (set-max-value-length (new-max uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        (var-set max-value-length new-max)
        (ok true)
    )
)