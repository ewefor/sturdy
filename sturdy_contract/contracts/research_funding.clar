
;; title: research_funding
;; version:
;; summary: Research Funding Smart Contract
;; description: Allows crowdfunding of research projects with milestone-based fund release

;; traits
;;

;; token definitions


(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-project (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-milestone-not-met (err u104))
(define-constant err-unauthorized (err u105))

;; Data structures
(define-map projects 
    { project-id: uint }
    {
        owner: principal,
        target-amount: uint,
        current-amount: uint,
        start-block: uint,
        end-block: uint,
        is-active: bool,
        total-milestones: uint,
        completed-milestones: uint
    }
)

(define-map project-milestones
    { project-id: uint, milestone-id: uint }
    {
        description: (string-ascii 256),
        amount: uint,
        is-completed: bool
    }
)

(define-map backers
    { project-id: uint, backer: principal }
    { amount: uint }
)

;; Project management functions
(define-public (create-project (project-id uint) (target-amount uint) (duration uint) (total-milestones uint))
    (let
        (
            (project-data {
                owner: tx-sender,
                target-amount: target-amount,
                current-amount: u0,
                start-block: block-height,
                end-block: (+ block-height duration),
                is-active: true,
                total-milestones: total-milestones,
                completed-milestones: u0
            })
        )
        (asserts! (is-none (map-get? projects { project-id: project-id })) err-already-exists)
        (ok (map-set projects { project-id: project-id } project-data))
    )
)

(define-public (add-milestone (project-id uint) (milestone-id uint) (description (string-ascii 256)) (amount uint))
    (let
        (
            (project (unwrap! (map-get? projects { project-id: project-id }) err-invalid-project))
        )
        (asserts! (is-eq tx-sender (get owner project)) err-owner-only)
        (ok (map-set project-milestones 
            { project-id: project-id, milestone-id: milestone-id }
            { description: description, amount: amount, is-completed: false }
        ))
    )
)

;; Funding functions
(define-public (fund-project (project-id uint) (amount uint))
    (let
        (
            (project (unwrap! (map-get? projects { project-id: project-id }) err-invalid-project))
            (current-backer-info (default-to { amount: u0 }
                (map-get? backers { project-id: project-id, backer: tx-sender })))
        )
        (asserts! (get is-active project) err-invalid-project)
        (asserts! (>= (stx-get-balance tx-sender) amount) err-insufficient-funds)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        ;; Update project funding
        (map-set projects { project-id: project-id }
            (merge project { current-amount: (+ (get current-amount project) amount) }))
        
        ;; Update backer information
        (ok (map-set backers 
            { project-id: project-id, backer: tx-sender }
            { amount: (+ (get amount current-backer-info) amount) }))
    )
)

;; Milestone completion and fund release
(define-public (complete-milestone (project-id uint) (milestone-id uint))
    (let
        (
            (project (unwrap! (map-get? projects { project-id: project-id }) err-invalid-project))
            (milestone (unwrap! (map-get? project-milestones { project-id: project-id, milestone-id: milestone-id }) 
                err-invalid-project))
        )
        (asserts! (is-eq (get owner project) tx-sender) err-unauthorized)
        (asserts! (not (get is-completed milestone)) err-milestone-not-met)
        
        ;; Update milestone status
        (map-set project-milestones 
            { project-id: project-id, milestone-id: milestone-id }
            (merge milestone { is-completed: true }))
        
        ;; Update project milestone count
        (map-set projects 
            { project-id: project-id }
            (merge project { completed-milestones: (+ (get completed-milestones project) u1) }))
        
        ;; Release funds for the milestone
        (try! (as-contract (stx-transfer? (get amount milestone) tx-sender (get owner project))))
        
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-project-details (project-id uint))
    (map-get? projects { project-id: project-id })
)

(define-read-only (get-milestone-details (project-id uint) (milestone-id uint))
    (map-get? project-milestones { project-id: project-id, milestone-id: milestone-id })
)

(define-read-only (get-backer-contribution (project-id uint) (backer principal))
    (map-get? backers { project-id: project-id, backer: backer })
)