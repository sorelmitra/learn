let leftBalloonCannon = scene.childNodeWithName("//left_cannon")!
let rightBalloonCannon = scene.childNodeWithName("//right_cannon")!

/* sorel */
var configureCannonPhysics: ((cannon: SKSpriteNode) -> Void)?
let cannonCategory: UInt32 = 2 << 1
configureCannonPhysics = { cannon in
    cannon.physicsBody = SKPhysicsBody(texture: cannon.texture, size: cannon.size)
    cannon.physicsBody!.linearDamping = 0.5
    cannon.physicsBody!.mass = 0.1
    cannon.physicsBody!.categoryBitMask = cannonCategory
    cannon.physicsBody!.contactTestBitMask = cannonCategory
}
configureCannonPhysics?(cannon: leftBalloonCannon as SKSpriteNode)
/* sorel */
