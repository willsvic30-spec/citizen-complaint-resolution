;; citizen-complaint-resolution
;; A municipal service platform for managing citizen complaints with issue categorization,
;; department routing, progress tracking, and satisfaction measurement

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-status (err u103))
(define-constant err-already-rated (err u104))
(define-constant err-invalid-rating (err u105))

;; complaint categories
(define-constant CATEGORY-INFRASTRUCTURE u1)
(define-constant CATEGORY-SANITATION u2)
(define-constant CATEGORY-PUBLIC-SAFETY u3)
(define-constant CATEGORY-UTILITIES u4)
(define-constant CATEGORY-OTHER u5)

;; complaint status
(define-constant STATUS-SUBMITTED u1)
(define-constant STATUS-ASSIGNED u2)
(define-constant STATUS-IN-PROGRESS u3)
(define-constant STATUS-RESOLVED u4)
(define-constant STATUS-CLOSED u5)

;; data vars
(define-data-var complaint-counter uint u0)

;; data maps

;; complaint data structure
(define-map complaints uint {
    citizen: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    category: uint,
    location: (string-ascii 200),
    status: uint,
    assigned-department: (optional (string-ascii 50)),
    created-at: uint,
    updated-at: uint,
    citizen-rating: (optional uint)
})

;; department assignments
(define-map department-assignments uint (string-ascii 50))

;; satisfaction ratings (1-5)
(define-map satisfaction-ratings uint uint)

;; department staff permissions
(define-map department-staff principal (string-ascii 50))

;; public functions

;; Submit a new complaint
(define-public (submit-complaint (title (string-ascii 100)) 
                                (description (string-ascii 500)) 
                                (category uint) 
                                (location (string-ascii 200)))
    (let ((complaint-id (+ (var-get complaint-counter) u1)))
        (asserts! (is-valid-category category) err-invalid-status)
        (map-set complaints complaint-id {
            citizen: tx-sender,
            title: title,
            description: description,
            category: category,
            location: location,
            status: STATUS-SUBMITTED,
            assigned-department: none,
            created-at: stacks-block-height,
            updated-at: stacks-block-height,
            citizen-rating: none
        })
        (var-set complaint-counter complaint-id)
        (ok complaint-id)
    )
)

;; Assign complaint to department (admin only)
(define-public (assign-to-department (complaint-id uint) (department (string-ascii 50)))
    (let ((complaint (unwrap! (map-get? complaints complaint-id) err-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set complaints complaint-id 
            (merge complaint {
                assigned-department: (some department),
                status: STATUS-ASSIGNED,
                updated-at: stacks-block-height
            })
        )
        (ok true)
    )
)

;; Add department staff member (admin only)
(define-public (add-department-staff (staff-member principal) (department (string-ascii 50)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set department-staff staff-member department)
        (ok true)
    )
)

;; Update complaint status (department staff or admin)
(define-public (update-status (complaint-id uint) (new-status uint))
    (let ((complaint (unwrap! (map-get? complaints complaint-id) err-not-found)))
        (asserts! (is-valid-status new-status) err-invalid-status)
        (asserts! (can-update-complaint complaint-id tx-sender) err-unauthorized)
        (map-set complaints complaint-id 
            (merge complaint {
                status: new-status,
                updated-at: stacks-block-height
            })
        )
        (ok true)
    )
)

;; Citizen rates the resolution (1-5 stars)
(define-public (rate-resolution (complaint-id uint) (rating uint))
    (let ((complaint (unwrap! (map-get? complaints complaint-id) err-not-found)))
        (asserts! (is-eq (get citizen complaint) tx-sender) err-unauthorized)
        (asserts! (>= (get status complaint) STATUS-RESOLVED) err-invalid-status)
        (asserts! (is-none (get citizen-rating complaint)) err-already-rated)
        (asserts! (is-valid-rating rating) err-invalid-rating)
        (map-set complaints complaint-id 
            (merge complaint {
                citizen-rating: (some rating),
                updated-at: stacks-block-height
            })
        )
        (ok true)
    )
)

;; read only functions

;; Get complaint details
(define-read-only (get-complaint (complaint-id uint))
    (map-get? complaints complaint-id)
)

;; Get total complaint count
(define-read-only (get-complaint-count)
    (var-get complaint-counter)
)

;; Get complaints by citizen
(define-read-only (get-citizen-complaints (citizen principal))
    ;; Note: This is a simplified version. In production, you'd implement pagination
    (list (map-get? complaints u1) (map-get? complaints u2) (map-get? complaints u3))
)

;; Get department assignment for staff member
(define-read-only (get-staff-department (staff-member principal))
    (map-get? department-staff staff-member)
)

;; Check if user can update a complaint
(define-read-only (can-user-update (complaint-id uint) (user principal))
    (can-update-complaint complaint-id user)
)

;; private functions

(define-private (is-valid-category (category uint))
    (and (>= category u1) (<= category u5))
)

(define-private (is-valid-status (status uint))
    (and (>= status u1) (<= status u5))
)

(define-private (is-valid-rating (rating uint))
    (and (>= rating u1) (<= rating u5))
)

(define-private (can-update-complaint (complaint-id uint) (user principal))
    (let ((complaint (unwrap! (map-get? complaints complaint-id) false))
          (user-department (map-get? department-staff user)))
        (or 
            (is-eq user contract-owner)
            (and (is-some user-department) 
                 (is-eq (get assigned-department complaint) user-department))
        )
    )
)
