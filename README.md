```mermaid
stateDiagram-v2
    [*] --> Empty
    Empty --> PendingVerification : UserRegistered
    PendingVerification --> Active : UserVerified
    PendingVerification --> Deleted : UserDeleted
    Active --> Locked : UserLocked
    Active --> Deleted : UserDeleted
    Locked --> Active : UserUnlocked
    Locked --> Deleted : UserDeleted
