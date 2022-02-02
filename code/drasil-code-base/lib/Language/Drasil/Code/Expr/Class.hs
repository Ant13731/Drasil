module Language.Drasil.Code.Expr.Class where
import Language.Drasil
import Language.Drasil.Chunk.CodeBase
import Control.Lens
import Language.Drasil.Code.Expr

class CodeExprC r where
    -- | Constructs a CodeExpr for actor creation (constructor call)
    new :: (Callable f, HasUID f, CodeIdea f) => f -> [r] -> r
    
    -- | Constructs a CodeExpr for actor creation (constructor call) that uses named arguments
    newWithNamedArgs :: (Callable f, HasUID f, CodeIdea f, HasUID a, 
      IsArgumentName a) => f -> [r] -> [(a, r)] -> r
    
    -- | Constructs a CodeExpr for actor messaging (method call)
    message :: (Callable f, HasUID f, CodeIdea f, HasUID c, HasSpace c, CodeIdea c) 
      => c -> f -> [r] -> r
    
    -- | Constructs a CodeExpr for actor messaging (method call) that uses named arguments
    msgWithNamedArgs :: (Callable f, HasUID f, CodeIdea f, HasUID c, HasSpace c, 
      CodeIdea c, HasUID a, IsArgumentName a) => c -> f -> [r] -> [(a, r)] -> 
      r
    
    -- | Constructs a CodeExpr representing the field of an actor
    field :: CodeVarChunk -> CodeVarChunk -> r


instance CodeExprC CodeExpr where
    new c ps = New (c ^. uid) ps []
    
    newWithNamedArgs c ps ns = New (c ^. uid) ps (zip (map ((^. uid) . fst) ns) 
      (map snd ns))

    message o m ps = checkObj (o ^. typ)
      where checkObj (Actor _) = Message (o ^. uid) (m ^. uid) ps []
            checkObj _ = error $ "Invalid actor message: Actor should have " ++ 
              "Actor space"
    
    msgWithNamedArgs o m ps as = checkObj (o ^. typ)
      where checkObj (Actor _) = Message (o ^. uid) (m ^. uid) ps 
              (zip (map ((^. uid) . fst) as) (map snd as))
            checkObj _ = error $ "Invalid actor message: Actor should have " ++ 
              "Actor space"
    
    field o f = checkObj (o ^. typ)
      where checkObj (Actor _) = Field (o ^. uid) (f ^. uid)
            checkObj _ = error $ "Invalid actor field: Actor should have " ++
              "Actor space"

