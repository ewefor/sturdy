;; Enhanced Voting Contract with Random Winner Selection and Advanced Features
;; This contract provides comprehensive voting functionality with multiple advanced features

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-VOTING-NOT-ACTIVE (err u101))
(define-constant ERR-VOTING-ENDED (err u102))
(define-constant ERR-ALREADY-VOTED (err u103))
(define-constant ERR-INVALID-CANDIDATE (err u104))
(define-constant ERR-NO-CANDIDATES (err u105))
(define-constant ERR-VOTING-STILL-ACTIVE (err u106))
(define-constant ERR-INSUFFICIENT-STAKE (err u107))
(define-constant ERR-DELEGATE-NOT-FOUND (err u108))
(define-constant ERR-INVALID-PROPOSAL (err u109))
(define-constant ERR-PROPOSAL-EXISTS (err u110))
(define-constant ERR-VOTING-NOT-STARTED (err u111))
(define-constant ERR-INVALID-QUORUM (err u112))
(define-constant ERR-QUORUM-NOT-MET (err u113))
(define-constant ERR-INVALID-WEIGHT (err u114))
(define-constant ERR-BLACKLISTED (err u115))
(define-constant ERR-VOTING-PAUSED (err u116))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MIN-STAKE-AMOUNT u1000000) ;; 1 STX in microSTX
(define-constant MAX-CANDIDATES u100)
(define-constant MAX-PROPOSALS u50)

;; Contract state - All data variables defined once here
(define-data-var voting-active bool false)
(define-data-var voting-start-block uint u0)
(define-data-var voting-end-block uint u0)
(define-data-var winner (optional principal) none)
(define-data-var total-votes uint u0)
(define-data-var total-weighted-votes uint u0)
(define-data-var voting-paused bool false)
(define-data-var emergency-stop bool false)
(define-data-var current-proposal-id uint u0)
(define-data-var candidate-count uint u0)
(define-data-var vote-history-count uint u0)
(define-data-var event-count uint u0)
(define-data-var total-supply uint u0)
(define-data-var whitelist-enabled bool false)

;; Quorum and thresholds
(define-data-var quorum-threshold uint u1000) ;; Minimum votes needed
(define-data-var winning-threshold uint u5000) ;; 50% in basis points (10000 = 100%)

;; Fee structure
(define-data-var voting-fee uint u10000) ;; Fee in microSTX to vote
(define-data-var registration-fee uint u100000) ;; Fee to register as candidate

;; Maps for basic voting
(define-map candidates principal {
  votes: uint,
  weighted-votes: uint,
  registration-block: uint,
  verified: bool,
  stake: uint
})

(define-map voters principal {
  has-voted: bool,
  vote-weight: uint,
  voted-for: (optional principal),
  vote-block: uint,
  stake: uint
})

(define-map candidate-list uint principal)

;; Weighted voting system
(define-map stake-weights principal uint) ;; principal -> voting weight based on stake
(define-map delegation principal principal) ;; delegator -> delegate
(define-map delegate-power principal uint) ;; delegate -> total delegated power

;; Multi-proposal system
(define-map proposals uint {
  title: (string-utf8 100),
  description: (string-utf8 500),
  proposer: principal,
  votes-for: uint,
  votes-against: uint,
  weighted-votes-for: uint,
  weighted-votes-against: uint,
  active: bool,
  executed: bool,
  creation-block: uint
})

(define-map proposal-votes { proposal-id: uint, voter: principal } {
  vote: bool, ;; true = for, false = against
  weight: uint,
  block: uint
})

;; Access control
(define-map admins principal bool)
(define-map blacklisted-users principal bool)
(define-map whitelisted-users principal bool)

;; Vote history and analytics
(define-map vote-history uint {
  voter: principal,
  candidate: (optional principal),
  proposal-id: (optional uint),
  weight: uint,
  block: uint,
  vote-type: (string-utf8 20)
})

;; Governance tokens (simple implementation)
(define-map token-balances principal uint)

;; Events/Logging
(define-map events uint {
  event-type: (string-utf8 50),
  data: (string-utf8 200),
  block: uint,
  sender: principal
})

;; Helper functions
(define-private (is-voting-active)
  (and 
    (var-get voting-active)
    (>= block-height (var-get voting-start-block))
    (<= block-height (var-get voting-end-block))
    (not (var-get voting-paused))
    (not (var-get emergency-stop))))

(define-private (is-admin (user principal))
  (or (is-eq user CONTRACT-OWNER)
      (default-to false (map-get? admins user))))

(define-private (is-blacklisted (user principal))
  (default-to false (map-get? blacklisted-users user)))

(define-private (is-whitelisted (user principal))
  (if (var-get whitelist-enabled)
    (default-to false (map-get? whitelisted-users user))
    true))

(define-private (can-participate (user principal))
  (and 
    (not (is-blacklisted user))
    (is-whitelisted user)))

(define-private (log-event (event-type (string-utf8 50)) (data (string-utf8 200)))
  (let ((event-id (var-get event-count)))
    (map-set events event-id {
      event-type: event-type,
      data: data,
      block: block-height,
      sender: tx-sender
    })
    (var-set event-count (+ event-id u1))))

;; Calculate voter weight (stake + delegated power)
(define-private (calculate-voter-weight (voter principal))
  (+ 
    (default-to u0 (map-get? stake-weights voter))
    (default-to u0 (map-get? delegate-power voter))))

;; Utility function to convert buffer to uint (simplified)
(define-private (buff-to-uint-simple (buff (buff 8)))
  (let ((byte0 (buff-to-uint-le (unwrap-panic (as-max-len? (concat 0x00000000000000000000000000000000 (unwrap-panic (slice? buff u0 u1))) u16))))
        (byte1 (buff-to-uint-le (unwrap-panic (as-max-len? (concat 0x00000000000000000000000000000000 (unwrap-panic (slice? buff u1 u1))) u16))))
        (byte2 (buff-to-uint-le (unwrap-panic (as-max-len? (concat 0x00000000000000000000000000000000 (unwrap-panic (slice? buff u2 u1))) u16))))
        (byte3 (buff-to-uint-le (unwrap-panic (as-max-len? (concat 0x00000000000000000000000000000000 (unwrap-panic (slice? buff u3 u1))) u16)))))
    (+ 
      (* byte0 u16777216)   ;; 256^3
      (* byte1 u65536)      ;; 256^2
      (* byte2 u256)        ;; 256^1
      byte3)))              ;; 256^0

;; Enhanced helper functions for weighted voting
(define-private (find-max-weighted-votes-fold (index uint) (max-votes uint))
  (if (>= index (var-get candidate-count))
    max-votes
    (match (map-get? candidate-list index)
      candidate (let ((candidate-data (default-to {votes: u0, weighted-votes: u0, registration-block: u0, verified: false, stake: u0} (map-get? candidates candidate)))
                      (weighted-votes (get weighted-votes candidate-data)))
        (if (> weighted-votes max-votes) weighted-votes max-votes))
      max-votes)))

(define-private (find-max-weighted-votes-iterative)
  (fold find-max-weighted-votes-fold (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32 u33 u34 u35 u36 u37 u38 u39 u40 u41 u42 u43 u44 u45 u46 u47 u48 u49 u50 u51 u52 u53 u54 u55 u56 u57 u58 u59 u60 u61 u62 u63 u64 u65 u66 u67 u68 u69 u70 u71 u72 u73 u74 u75 u76 u77 u78 u79 u80 u81 u82 u83 u84 u85 u86 u87 u88 u89 u90 u91 u92 u93 u94 u95 u96 u97 u98 u99) u0))

(define-private (collect-tied-candidates-fold (index uint) (acc { max-votes: uint, tied-candidates: (list 100 principal) }))
  (if (>= index (var-get candidate-count))
    acc
    (match (map-get? candidate-list index)
      candidate (let ((candidate-data (default-to {votes: u0, weighted-votes: u0, registration-block: u0, verified: false, stake: u0} (map-get? candidates candidate)))
                      (weighted-votes (get weighted-votes candidate-data)))
        (if (is-eq weighted-votes (get max-votes acc))
          { max-votes: (get max-votes acc), 
            tied-candidates: (unwrap-panic (as-max-len? (append (get tied-candidates acc) candidate) u100)) }
          acc))
      acc)))

(define-private (collect-tied-weighted-candidates-iterative (max-votes uint))
  (fold collect-tied-candidates-fold (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32 u33 u34 u35 u36 u37 u38 u39 u40 u41 u42 u43 u44 u45 u46 u47 u48 u49 u50 u51 u52 u53 u54 u55 u56 u57 u58 u59 u60 u61 u62 u63 u64 u65 u66 u67 u68 u69 u70 u71 u72 u73 u74 u75 u76 u77 u78 u79 u80 u81 u82 u83 u84 u85 u86 u87 u88 u89 u90 u91 u92 u93 u94 u95 u96 u97 u98 u99) { max-votes: max-votes, tied-candidates: (list) }))

;; Enhanced randomness with multiple entropy sources
(define-private (get-enhanced-random-seed)
  (let ((block-hash (unwrap-panic (get-block-info? id-header-hash (- block-height u1))))
        (current-height block-height)
        (total-votes-count (var-get total-votes))
        (total-weighted-count (var-get total-weighted-votes))
        (candidate-count-val (var-get candidate-count)))
    (+ 
      (mod (buff-to-uint-simple (unwrap-panic (as-max-len? (unwrap-panic (slice? block-hash u0 u8)) u8))) u1000000)
      (mod current-height u1000000)
      (mod total-votes-count u1000000)
      (mod total-weighted-count u1000000)
      (mod candidate-count-val u1000000)
      (mod (var-get vote-history-count) u1000000))))

(define-private (select-random-winner (tied-candidates (list 100 principal)))
  (let ((candidates-count (len tied-candidates))
        (random-seed (get-enhanced-random-seed))
        (winner-index (mod random-seed candidates-count)))
    (element-at tied-candidates winner-index)))

;; Staking system for weighted voting
(define-public (stake-tokens (amount uint))
  (begin
    (asserts! (can-participate tx-sender) ERR-BLACKLISTED)
    (asserts! (>= amount MIN-STAKE-AMOUNT) ERR-INSUFFICIENT-STAKE)
    
    ;; Transfer STX to contract (simplified - in production use STX transfer)
    (let ((current-weight (default-to u0 (map-get? stake-weights tx-sender))))
      (map-set stake-weights tx-sender (+ current-weight amount))
      (log-event u"STAKE" u"Tokens staked for voting weight")
      (ok amount))))

(define-public (unstake-tokens (amount uint))
  (let ((current-weight (default-to u0 (map-get? stake-weights tx-sender))))
    (asserts! (>= current-weight amount) ERR-INSUFFICIENT-STAKE)
    (asserts! (not (is-voting-active)) ERR-VOTING-NOT-ACTIVE)
    
    (map-set stake-weights tx-sender (- current-weight amount))
    (log-event u"UNSTAKE" u"Tokens unstaked")
    (ok amount)))

;; Delegation system
(define-public (delegate-vote (delegate principal))
  (begin
    (asserts! (can-participate tx-sender) ERR-BLACKLISTED)
    (asserts! (can-participate delegate) ERR-BLACKLISTED)
    (asserts! (not (is-eq tx-sender delegate)) ERR-INVALID-CANDIDATE)
    
    (let ((delegator-weight (default-to u0 (map-get? stake-weights tx-sender)))
          (current-delegate-power (default-to u0 (map-get? delegate-power delegate))))
      (map-set delegation tx-sender delegate)
      (map-set delegate-power delegate (+ current-delegate-power delegator-weight))
      (log-event u"DELEGATE" u"Vote delegated")
      (ok true))))

(define-public (undelegate-vote)
  (match (map-get? delegation tx-sender)
    delegate (let ((delegator-weight (default-to u0 (map-get? stake-weights tx-sender)))
                   (current-delegate-power (default-to u0 (map-get? delegate-power delegate))))
               (map-delete delegation tx-sender)
               (map-set delegate-power delegate (- current-delegate-power delegator-weight))
               (log-event u"UNDELEGATE" u"Vote undelegated")
               (ok true))
    ERR-DELEGATE-NOT-FOUND))

;; Enhanced candidate management
(define-public (register-as-candidate)
  (begin
    (asserts! (can-participate tx-sender) ERR-BLACKLISTED)
    (asserts! (not (is-voting-active)) ERR-VOTING-NOT-ACTIVE)
    (asserts! (< (var-get candidate-count) MAX-CANDIDATES) ERR-INVALID-CANDIDATE)
    (asserts! (is-none (map-get? candidates tx-sender)) ERR-INVALID-CANDIDATE)
    
    ;; Require registration fee and stake
    (try! (stake-tokens (var-get registration-fee)))
    
    (let ((current-count (var-get candidate-count)))
      (map-set candidates tx-sender {
        votes: u0,
        weighted-votes: u0,
        registration-block: block-height,
        verified: false,
        stake: (var-get registration-fee)
      })
      (map-set candidate-list current-count tx-sender)
      (var-set candidate-count (+ current-count u1))
      (log-event u"CANDIDATE_REGISTERED" u"New candidate registered")
      (ok true))))

(define-public (verify-candidate (candidate principal))
  (begin
    (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? candidates candidate)) ERR-INVALID-CANDIDATE)
    
    (map-set candidates candidate 
      (merge (unwrap-panic (map-get? candidates candidate)) { verified: true }))
    (log-event u"CANDIDATE_VERIFIED" u"Candidate verified by admin")
    (ok true)))

;; Multi-proposal system
(define-public (create-proposal (title (string-utf8 100)) (description (string-utf8 500)))
  (begin
    (asserts! (can-participate tx-sender) ERR-BLACKLISTED)
    (asserts! (< (var-get current-proposal-id) MAX-PROPOSALS) ERR-INVALID-PROPOSAL)
    
    (let ((proposal-id (var-get current-proposal-id)))
      (map-set proposals proposal-id {
        title: title,
        description: description,
        proposer: tx-sender,
        votes-for: u0,
        votes-against: u0,
        weighted-votes-for: u0,
        weighted-votes-against: u0,
        active: true,
        executed: false,
        creation-block: block-height
      })
      (var-set current-proposal-id (+ proposal-id u1))
      (log-event u"PROPOSAL_CREATED" title)
      (ok proposal-id))))

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (begin
    (asserts! (is-voting-active) ERR-VOTING-NOT-ACTIVE)
    (asserts! (can-participate tx-sender) ERR-BLACKLISTED)
    (asserts! (< proposal-id (var-get current-proposal-id)) ERR-INVALID-PROPOSAL)
    
    (let ((voter-weight (calculate-voter-weight tx-sender))
          (proposal-data (unwrap! (map-get? proposals proposal-id) ERR-INVALID-PROPOSAL)))
      
      (asserts! (get active proposal-data) ERR-INVALID-PROPOSAL)
      (asserts! (is-none (map-get? proposal-votes { proposal-id: proposal-id, voter: tx-sender })) ERR-ALREADY-VOTED)
      
      ;; Record vote
      (map-set proposal-votes { proposal-id: proposal-id, voter: tx-sender } {
        vote: vote-for,
        weight: voter-weight,
        block: block-height
      })
      
      ;; Update proposal totals
      (if vote-for
        (map-set proposals proposal-id 
          (merge proposal-data {
            votes-for: (+ (get votes-for proposal-data) u1),
            weighted-votes-for: (+ (get weighted-votes-for proposal-data) voter-weight)
          }))
        (map-set proposals proposal-id 
          (merge proposal-data {
            votes-against: (+ (get votes-against proposal-data) u1),
            weighted-votes-against: (+ (get weighted-votes-against proposal-data) voter-weight)
          })))
      
      (log-event u"PROPOSAL_VOTE" u"Vote cast on proposal")
      (ok true))))

;; Enhanced voting with fees and weights
(define-public (vote-with-fee (candidate principal))
  (begin
    (asserts! (is-voting-active) ERR-VOTING-NOT-ACTIVE)
    (asserts! (can-participate tx-sender) ERR-BLACKLISTED)
    
    (let ((voter-data (map-get? voters tx-sender))
          (voter-weight (calculate-voter-weight tx-sender)))
      
      (asserts! (is-none voter-data) ERR-ALREADY-VOTED)
      (asserts! (is-some (map-get? candidates candidate)) ERR-INVALID-CANDIDATE)
      
      ;; Charge voting fee (simplified - in production use STX transfer)
      (asserts! (>= voter-weight (var-get voting-fee)) ERR-INSUFFICIENT-STAKE)
      
      ;; Record vote
      (map-set voters tx-sender {
        has-voted: true,
        vote-weight: voter-weight,
        voted-for: (some candidate),
        vote-block: block-height,
        stake: voter-weight
      })
      
      ;; Update candidate totals
      (let ((candidate-data (unwrap-panic (map-get? candidates candidate))))
        (map-set candidates candidate 
          (merge candidate-data {
            votes: (+ (get votes candidate-data) u1),
            weighted-votes: (+ (get weighted-votes candidate-data) voter-weight)
          })))
      
      ;; Update global totals
      (var-set total-votes (+ (var-get total-votes) u1))
      (var-set total-weighted-votes (+ (var-get total-weighted-votes) voter-weight))
      
      ;; Log vote in history
      (let ((history-id (var-get vote-history-count)))
        (map-set vote-history history-id {
          voter: tx-sender,
          candidate: (some candidate),
          proposal-id: none,
          weight: voter-weight,
          block: block-height,
          vote-type: u"CANDIDATE"
        })
        (var-set vote-history-count (+ history-id u1)))
      
      (log-event u"VOTE_CAST" u"Weighted vote cast with fee")
      (ok voter-weight))))

;; Advanced voting configuration
(define-public (start-advanced-voting 
  (duration-blocks uint) 
  (quorum uint) 
  (threshold uint) 
  (enable-whitelist bool))
  (begin
    (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (not (var-get voting-active)) ERR-VOTING-NOT-ACTIVE)
    (asserts! (> (var-get candidate-count) u0) ERR-NO-CANDIDATES)
    (asserts! (<= threshold u10000) ERR-INVALID-WEIGHT)
    
    (var-set voting-active true)
    (var-set voting-start-block block-height)
    (var-set voting-end-block (+ block-height duration-blocks))
    (var-set quorum-threshold quorum)
    (var-set winning-threshold threshold)
    (var-set whitelist-enabled enable-whitelist)
    (var-set winner none)
    (var-set total-votes u0)
    (var-set total-weighted-votes u0)
    
    (log-event u"VOTING_STARTED" u"Advanced voting session started")
    (ok true)))

;; Emergency controls
(define-public (pause-voting)
  (begin
    (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
    (var-set voting-paused true)
    (log-event u"VOTING_PAUSED" u"Voting paused by admin")
    (ok true)))

(define-public (resume-voting)
  (begin
    (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
    (var-set voting-paused false)
    (log-event u"VOTING_RESUMED" u"Voting resumed by admin")
    (ok true)))

(define-public (activate-emergency-stop)
  (begin
    (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
    (var-set emergency-stop true)
    (var-set voting-active false)
    (log-event u"EMERGENCY_STOP" u"Emergency stop activated")
    (ok true)))

;; Access control management
(define-public (add-admin (admin principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set admins admin true)
    (log-event u"ADMIN_ADDED" u"New admin added")
    (ok true)))

(define-public (blacklist-user (user principal))
  (begin
    (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
    (map-set blacklisted-users user true)
    (log-event u"USER_BLACKLISTED" u"User blacklisted")
    (ok true)))

(define-public (whitelist-user (user principal))
  (begin
    (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
    (map-set whitelisted-users user true)
    (log-event u"USER_WHITELISTED" u"User whitelisted")
    (ok true)))

;; Enhanced winner determination with quorum and threshold checks
(define-public (finalize-voting)
  (begin
    (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (var-get voting-active) ERR-VOTING-NOT-ACTIVE)
    (asserts! (> block-height (var-get voting-end-block)) ERR-VOTING-STILL-ACTIVE)
    
    ;; Check quorum
    (asserts! (>= (var-get total-votes) (var-get quorum-threshold)) ERR-QUORUM-NOT-MET)
    
    (let ((max-weighted-votes (find-max-weighted-votes-iterative)))
      (if (is-eq max-weighted-votes u0)
        ;; No votes cast
        (begin
          (var-set voting-active false)
          (var-set winner none)
          (log-event u"VOTING_ENDED" u"No winner - no votes")
          (ok none))
        ;; Check if winner meets threshold
        (let ((winner-percentage (* (/ max-weighted-votes (var-get total-weighted-votes)) u10000)))
          (if (>= winner-percentage (var-get winning-threshold))
            ;; Find tied candidates and select winner
            (let ((tied-result (collect-tied-weighted-candidates-iterative max-weighted-votes))
                  (tied-candidates (get tied-candidates tied-result)))
              (if (is-eq (len tied-candidates) u1)
                ;; Single winner
                (begin
                  (var-set voting-active false)
                  (var-set winner (element-at tied-candidates u0))
                  (log-event u"VOTING_ENDED" u"Winner determined")
                  (ok (element-at tied-candidates u0)))
                ;; Multiple tied candidates - random selection
                (let ((selected-winner (select-random-winner tied-candidates)))
                  (var-set voting-active false)
                  (var-set winner selected-winner)
                  (log-event u"VOTING_ENDED" u"Winner selected randomly from tie")
                  (ok selected-winner))))
            ;; No winner meets threshold
            (begin
              (var-set voting-active false)
              (var-set winner none)
              (log-event u"VOTING_ENDED" u"No winner meets threshold")
              (ok none))))))))

;; Comprehensive read-only functions
(define-read-only (get-comprehensive-status)
  {
    voting: {
      active: (is-voting-active),
      start-block: (var-get voting-start-block),
      end-block: (var-get voting-end-block),
      paused: (var-get voting-paused),
      emergency-stopped: (var-get emergency-stop)
    },
    totals: {
      votes: (var-get total-votes),
      weighted-votes: (var-get total-weighted-votes),
      candidates: (var-get candidate-count),
      proposals: (var-get current-proposal-id)
    },
    settings: {
      quorum: (var-get quorum-threshold),
      threshold: (var-get winning-threshold),
      voting-fee: (var-get voting-fee),
      whitelist-enabled: (var-get whitelist-enabled)
    },
    winner: (var-get winner)
  })

(define-read-only (get-candidate-details (candidate principal))
  (map-get? candidates candidate))

(define-read-only (get-voter-details (voter principal))
  (map-get? voters voter))

(define-read-only (get-proposal-details (proposal-id uint))
  (map-get? proposals proposal-id))

(define-read-only (get-vote-history (vote-id uint))
  (map-get? vote-history vote-id))

(define-read-only (get-user-permissions (user principal))
  {
    is-admin: (is-admin user),
    is-blacklisted: (is-blacklisted user),
    is-whitelisted: (is-whitelisted user),
    can-participate: (can-participate user),
    stake-weight: (default-to u0 (map-get? stake-weights user)),
    delegate-power: (default-to u0 (map-get? delegate-power user))
  })

(define-read-only (get-delegation-info (user principal))
  {
    delegated-to: (map-get? delegation user),
    delegated-power: (default-to u0 (map-get? delegate-power user))
  })

;; Analytics functions
(define-read-only (get-voting-analytics)
  (let ((total-weight (var-get total-weighted-votes))
        (total-count (var-get total-votes)))
    {
      participation-rate: (if (> total-count u0) 
        (/ (* total-count u10000) (var-get candidate-count)) u0),
      average-vote-weight: (if (> total-count u0) (/ total-weight total-count) u0),
      quorum-met: (>= total-count (var-get quorum-threshold)),
      voting-events: (var-get event-count)
    }))