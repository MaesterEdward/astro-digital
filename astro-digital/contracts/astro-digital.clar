;; AstroDigita;; AstroDigital - Creator Economy Platform
;; Simplified smart contract for digital asset creation and revenue sharing

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-price (err u104))

;; Data Variables
(define-data-var next-asset-id uint u1)
(define-data-var platform-fee-rate uint u250) ;; 2.5% = 250 basis points

;; Data Maps
(define-map assets
  uint ;; asset-id
  {
    creator: principal,
    asset-type: (string-ascii 20), ;; "star" or "satellite"  
    parent-id: (optional uint), ;; for satellite assets
    title: (string-utf8 100),
    price: uint,
    creation-hash: (buff 32), ;; creation signature hash
    is-active: bool
  }
)

(define-map creator-profiles
  principal
  {
    reputation-score: uint,
    total-sales: uint,
    verified: bool,
    creation-count: uint
  }
)

(define-map asset-ownership
  {asset-id: uint, owner: principal}
  {
    owned: bool,
    purchase-price: uint,
    purchase-block: uint
  }
)

(define-map constellation-links
  {star-id: uint, satellite-id: uint}
  {
    revenue-share: uint, ;; percentage to star creator
    linked-block: uint
  }
)

;; Read-only functions
(define-read-only (get-asset (asset-id uint))
  (map-get? assets asset-id)
)

(define-read-only (get-creator-profile (creator principal))
  (map-get? creator-profiles creator)
)

(define-read-only (get-asset-ownership (asset-id uint) (owner principal))
  (map-get? asset-ownership {asset-id: asset-id, owner: owner})
)

(define-read-only (get-constellation-link (star-id uint) (satellite-id uint))
  (map-get? constellation-links {star-id: star-id, satellite-id: satellite-id})
)

(define-read-only (get-next-asset-id)
  (var-get next-asset-id)
)

;; Create a new Star Asset (primary work)
(define-public (create-star-asset 
  (title (string-utf8 100))
  (price uint)
  (creation-hash (buff 32)))
  (let ((asset-id (var-get next-asset-id)))
    (asserts! (> price u0) err-invalid-price)
    (map-set assets asset-id {
      creator: tx-sender,
      asset-type: "star",
      parent-id: none,
      title: title,
      price: price,
      creation-hash: creation-hash,
      is-active: true
    })
    (var-set next-asset-id (+ asset-id u1))
    (update-creator-creation-count tx-sender)
    (ok asset-id)
  )
)

;; Create a new Satellite Asset (derivative work)
(define-public (create-satellite-asset
  (title (string-utf8 100))
  (price uint)
  (creation-hash (buff 32))
  (parent-star-id uint)
  (star-revenue-share uint)) ;; percentage to star creator
  (let ((asset-id (var-get next-asset-id))
        (parent-asset (unwrap! (map-get? assets parent-star-id) err-not-found)))
    (asserts! (> price u0) err-invalid-price)
    (asserts! (<= star-revenue-share u5000) err-invalid-price) ;; max 50%
    (asserts! (is-eq (get asset-type parent-asset) "star") err-unauthorized)
    
    (map-set assets asset-id {
      creator: tx-sender,
      asset-type: "satellite",
      parent-id: (some parent-star-id),
      title: title,
      price: price,
      creation-hash: creation-hash,
      is-active: true
    })
    
    ;; Link to constellation
    (map-set constellation-links 
      {star-id: parent-star-id, satellite-id: asset-id}
      {revenue-share: star-revenue-share, linked-block: block-height})
    
    (var-set next-asset-id (+ asset-id u1))
    (update-creator-creation-count tx-sender)
    (ok asset-id)
  )
)

;; Purchase an asset
(define-public (purchase-asset (asset-id uint))
  (let ((asset-data (unwrap! (map-get? assets asset-id) err-not-found))
        (price (get price asset-data))
        (creator (get creator asset-data)))
    (asserts! (get is-active asset-data) err-unauthorized)
    
    ;; Transfer payment to creator
    (try! (stx-transfer? price tx-sender creator))
    
    ;; Record ownership
    (map-set asset-ownership 
      {asset-id: asset-id, owner: tx-sender}
      {owned: true, purchase-price: price, purchase-block: block-height})
    
    ;; Update creator sales
    (update-creator-sales-data creator price)
    
    ;; Handle satellite revenue sharing if applicable
    (match (get parent-id asset-data)
      parent-star-id (handle-revenue-sharing asset-id parent-star-id price)
      true)
    
    (ok true)
  )
)

;; Handle revenue sharing for satellite assets
(define-private (handle-revenue-sharing (satellite-id uint) (star-id uint) (sale-price uint))
  (match (map-get? constellation-links {star-id: star-id, satellite-id: satellite-id})
    link-data 
      (match (map-get? assets star-id)
        star-asset 
          (let ((star-share (/ (* sale-price (get revenue-share link-data)) u10000)))
            (if (> star-share u0)
              (begin
                (unwrap-panic (stx-transfer? star-share tx-sender (get creator star-asset)))
                true)
              true))
        true)
    true)
)

;; Update creator creation count
(define-private (update-creator-creation-count (creator principal))
  (let ((current-profile (default-to 
                          {reputation-score: u100, total-sales: u0, verified: false, creation-count: u0}
                          (map-get? creator-profiles creator))))
    (map-set creator-profiles creator {
      reputation-score: (get reputation-score current-profile),
      total-sales: (get total-sales current-profile),
      verified: (get verified current-profile),
      creation-count: (+ (get creation-count current-profile) u1)
    })
  )
)

;; Update creator sales data
(define-private (update-creator-sales-data (creator principal) (sale-amount uint))
  (let ((current-profile (default-to 
                          {reputation-score: u100, total-sales: u0, verified: false, creation-count: u0}
                          (map-get? creator-profiles creator))))
    (map-set creator-profiles creator {
      reputation-score: (if (> (+ (get reputation-score current-profile) u10) u1000) 
                          u1000 
                          (+ (get reputation-score current-profile) u10)),
      total-sales: (+ (get total-sales current-profile) sale-amount),
      verified: (get verified current-profile),
      creation-count: (get creation-count current-profile)
    })
  )
)

;; Deactivate an asset (creator only)
(define-public (deactivate-asset (asset-id uint))
  (let ((asset-data (unwrap! (map-get? assets asset-id) err-not-found)))
    (asserts! (is-eq tx-sender (get creator asset-data)) err-unauthorized)
    (map-set assets asset-id (merge asset-data {is-active: false}))
    (ok true)
  )
)