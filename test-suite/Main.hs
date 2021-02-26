-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec
    ( describe, it, parallel, shouldReturn, testSpec, Spec )
import Control.Monad ( ap, liftM2, liftM )
import Control.Concurrent.STM
    ( STM, atomically, newTVar, readTVar )
import Practica
    ( giveItem,
      sellItem,
      transfer,
      HitPoint,
      InventoryOut,
      Item(RedScroll, Axe),
      Player(Player, inventory, account) )

newPlayer :: Int -> HitPoint -> InventoryOut -> STM Player
newPlayer balance playerHealth playerInventory =
    Player `liftM` newTVar balance
              `ap` newTVar playerHealth
              `ap` newTVar playerInventory

transferTest :: STM (Int, Int)
transferTest = do
  caballero <- newTVar 30
  mago <- newTVar 10
  transfer 10 caballero mago
  -- Lifteo el valor de dos resultados separados por coma
  liftM2 (,) (readTVar caballero) (readTVar mago) 

giveItemTest :: STM ([Item], [Item])
giveItemTest = do
    caballero <- newPlayer 20 (100 :: HitPoint) [Axe, RedScroll]
    mago <- newPlayer 20 (100 :: HitPoint) []
    _ <- giveItem RedScroll (inventory caballero) (inventory mago)
    liftM2 (,) (readTVar (inventory caballero)) (readTVar (inventory mago)) 

sellItemTestCheckInventory :: STM ([Item], [Item])
sellItemTestCheckInventory = do
    caballero <- newPlayer 20 (100 :: HitPoint) [Axe, RedScroll]
    mago <- newPlayer 20 (100 :: HitPoint) []
    _ <- sellItem RedScroll 10 mago caballero
    liftM2 (,) (readTVar (inventory caballero)) (readTVar (inventory mago))

sellItemTestCheckAccount :: STM (Int, Int)
sellItemTestCheckAccount = do
    caballero <- newPlayer 20 (100 :: HitPoint) [Axe, RedScroll]
    mago <- newPlayer 20 (100 :: HitPoint) []
    _ <- sellItem RedScroll 10 mago caballero
    liftM2 (,) (readTVar (account caballero)) (readTVar (account mago))
    
main :: IO ()
main = do
    test <- testSpec "iasc-stm-practica" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
    describe "transferencia de dinero" $ do
        it "Transfer Test" $ do
            atomically (transferTest) `shouldReturn` (20, 20)

    describe "give Item" $ do
        it "Should be able to transfer an item" $ do
            atomically (giveItemTest) `shouldReturn` ([Axe], [RedScroll])

    describe "sell Item" $ do
        it "Should transfer the item sold" $ do
            atomically (sellItemTestCheckInventory) `shouldReturn` ([Axe], [RedScroll])
        
        it "Should transfer the gold from the sold" $ do
            atomically (sellItemTestCheckAccount) `shouldReturn` (30, 10)