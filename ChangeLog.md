
# ChangeLog

## [(diff)](https://github.com/haskell-nix/hnix/compare/0.16.0...0.17.0#files_bucket) 0.17.0

* Breaking:
  * `Nix.Expr.Types`
    * [(link)](https://github.com/haskell-nix/hnix/pull/1042/files) The central HNix type `NExprF` changed, the `NApp` was moved out of `NBinary` & now a `NExprF` constructor of its own, the type signatures were changed accordingly.
    * [(link)](https://github.com/haskell-nix/hnix/pull/1038/files) project was using `megaparsec` `{,Source}Pos` and to use it shipped a lot of orphan instances. To improve the situation & performance (reports [#1026](https://github.com/haskell-nix/hnix/issues/1026), [#746](https://github.com/haskell-nix/hnix/issues/746)) project uses `N{,Source}Pos` types, related type signatures were changed accordingly.
  * `Nix.Value`
    * [(link)](https://github.com/haskell-nix/hnix/pull/1046/files) Unify builder `mkNV*` and `NV*` patterns by bidirectional synonyms, a lot of builders `mkNV*` are removed, and merged to `NV*`. e.g. instead of builder `mkNVList`, `NVList` should be used.
    * [(link)](https://github.com/haskell-nix/hnix/pull/1046/files) Constraint `NVConstraint f = (Comonad f, Applicative f)` was introduced, in order to unify builder `mkNV*` and `NV*` patterns.

  * `Nix.Parser`:
    * [(link)](https://github.com/haskell-nix/hnix/pull/1047/files) rm `OperatorInfo`, using `NOperatorDef`. Number of functions changed signatures accordingly:
    * In `Nix.Pretty`:
      * `NixDoc ann`
      * `mkNixDoc`
      * `selectOp`
      * `hasAttrOp`
      * `precedenceWrap`
      * `wrapPath`
    * In `Nix.Parser`:
      * rm `get{App,Unary,Binary,Special}Operator`, currely `NOp` class instances are used instead.
    
  * `Nix.Pretty`:
    * [(link)](https://github.com/haskell-nix/hnix/pull/1047/files) rm `appOp`, instead use `appOpDef`.
    * [(link)](https://github.com/haskell-nix/hnix/pull/1047/files) `precedenceWrap` behaviour is changed (to be literal to the name), the old behaviour is now a `wrap` function.

## [(diff)](https://github.com/haskell-nix/hnix/compare/0.15.0...0.16.0#files_bucket) 0.16.0

On update problems, please reach out to us. For support refere to: https://github.com/haskell-nix/hnix/issues/984

Partial log (for now):

* Breaking:
  * Where `coerce` should work, removed `newtype` accessors.
  * [(link)](https://github.com/haskell-nix/hnix/pull/1006/files), [(link)](https://github.com/haskell-nix/hnix/pull/1009/files) Incomprehensible record accessors zoo like: `arg`, `options`, `unStore`, `scFlavor`, `nsContext` `_provenance` - was organized, now all record accessors start with `get*`, and their names tend to have according unique sematic meaning of data action they do.
  * Builder names got unified. Now they all start with `mk*`. So a lof of `nvSet` became `mkNVSet`.
  * `Nix.String` builders/destructors instead of `make` use `mk`, & where mentioning of `string` is superflous - dropped it from the name, so `stringIgnoreContext`, became `ignoreContext`.
  * Type system:
    * Things that are paths are now `newtype Path = Path String`.
    * Things that are indentifier names are now `newtype VarName = VarName Text`.
    * Function signatures changed accordingly.

* Additional:
  * [(link)](https://github.com/haskell-nix/hnix/pull/1019) Matched expression escaping & its representation to breaking changes in Nix `2.4`.
  * `Builtins` (`builtins` function set) gained functions:
    * [(link)](https://github.com/haskell-nix/hnix/pull/1032) `path`
    * [(link)](https://github.com/haskell-nix/hnix/pull/1020) `isPathNix`
    * [(link)](https://github.com/haskell-nix/hnix/pull/1032) `unsafeDiscardOutputDependency`
    * [(link)](https://github.com/haskell-nix/hnix/pull/1031) `ceil`
    * [(link)](https://github.com/haskell-nix/hnix/pull/1031) `floor`
    * [(link)](https://github.com/haskell-nix/hnix/pull/1021) `hashFile`
    * [(link)](https://github.com/haskell-nix/hnix/pull/1033) `groupBy`
  * [(link)](https://github.com/haskell-nix/hnix/pull/1029) `data/nix` submodule (& its tests) updated to 2022-01-17.

* Other notes:
  * `Shorthands` was kept untouched.

## [(diff)](https://github.com/haskell-nix/hnix/compare/0.14.0...0.15.0#files_bucket) 0.15.0

For support refere to: https://github.com/haskell-nix/hnix/issues/984

Partial log (for now):

* Breaking:

  * `Nix.Expr.Shorthands`:
    * `inherit{,From}`:
      * dropped second(/third) argument as irrelevant ([report](https://github.com/haskell-nix/hnix/issues/326))
      * bindings to inherit changed type from complex `[NKeyName]` (which is for static & dynamic keys) to `[VarName]` (`VarName` is newtype of `Text`).
      * So examples of use now are: `inherit ["a", "b"]`, `inheritFrom (var "a") ["b", "c"]`
    * `mkAssert`: fixed ([report](https://github.com/haskell-nix/hnix/issues/969)).
    * fx presedence between the operators:
        
        ```haskell
        (@@), (@.), (@./), ($==), ($!=), ($<), ($<=), ($>), ($>=), ($&&), ($||), ($->), ($//), ($+), ($-), ($*), ($/), ($++), (==>)
        ```
        
        Now these shorthands can be used without sectioning & so represent the Nix expressions one to one.
        
        ```haskell
        nix = "           a/b   //            c/def   //           <g>  <             def/d"
        hask = mkRelPath "a/b" $// mkRelPath "c/def" $// mkEnvPath "g" $<  mkRelPath "def/d"
        ```

* Additional
  * `Nix.Expr.Shorthands`:
    * added:
      * `emptySet`
      * `emptyList`
      * `mkOp{,2}`
      * `mk{,Named,Variadic,General}ParamSet`
      * `mkNeg` - number negation.
      * `@.<|>` for Nix language `s.x or y` expession.
    * entered deprecation:
      * `mkOper{,2}` bacame `mkOp{,2}`.
      * `mkBinop` became `mkOp2`.
      * `mkParaset` supeceeded by `mk{,Named{,Variadic},Variadic,General}ParamSet`.
    * fixed:
      * `mkAssert` was creating `with`, now properly creates `assert`.

## [(diff)](https://github.com/haskell-nix/hnix/compare/0.13.1...0.14.0#files_bucket) 0.14.0 (2021-07-08)

* GHC 9.0 support.

* HNix switched to pre-0.9 style of log (aka "no log"). We temporarily stopped producing log, choosing effectiveness over writing about it.

* All changes seem trivial (from the inside). There is no changes in `Nix.Expr.Shorthands` module. Would there be difficulties in migration - please write to us - we would tackle & solve it togather.

A partial log:

* Breaking:

  * `Nix.Effects`:
    * rm `pathExits` in favour of `doesPathExist` (in `Nix.Render`: `class MonadFile`: `doesPathExist`)

  * `Nix.Var`: was found being superflous ([report](https://github.com/haskell-nix/hnix/issues/946)), so reduced. use `Control.Monad.Ref` instead.

  * `Nix.Normal`
    * rename `opaque(,->Val)`, indicate that it is a literal.
  
  * `Nix.Thunk`:
    * `class MonadThunkId m => MonadThunk{,F} t m a`:
      * rename `query(M->){,F}`

* Additional:

  * `Nix.Utils`:
    * added type `TransformF`

  * `Nix.Eval`:
    * added fun:
      * `evalContent`
      * `addMetaInfo`
      
  * `Nix.Types.Assumption`:
    * added instances:
      * `Assumption`: `{Semigroup,Monoid,One}`

  * `Nix.Type.Env`:
    * added instances:
      * `Env`: `{Semigroup,Monoid,One}`
  
  * `Nix`:
    * changed argument order:
      * `nixEval`:
        
        ```haskell
        -- was:
          => Maybe FilePath -> Transform g (m a) -> Alg g (m a) -> Fix g -> m a
        -- became:
          => Transform g (m a) -> Alg g (m a) -> Maybe FilePath -> Fix g -> m a
        ```
        
  * `Nix.Normal`
    * add `thunkVal` literal & use it where appropriate `{deThunk, removeEffects}`
      
  * `Nix.Thunk.Basic`:
    * export `deferred`


### [(diff)](https://github.com/haskell-nix/hnix/compare/0.13.0.1...0.13.1#files_bucket) 0.13.1 (2021-05-22)
  * [(link)](https://github.com/haskell-nix/hnix/pull/936/files) `Nix.Parser`: `annotateLocation`: Fix source location preservation.
  * [(link)](https://github.com/haskell-nix/hnix/pull/934/files) Require Cabal dependency `relude` `>= 1.0`: since imports & Cabal file got cleaned-up & that clean-up depends on `relude` reimports introduced in aforementioned version.
  * Refactors, reorganization in some modules, docs, clean-ups.

#### [(diff)](https://github.com/haskell-nix/hnix/compare/0.13.0...0.13.0.1#files_bucket) 0.13.0.1 (2021-05-11)
  * [(link)](https://github.com/haskell-nix/hnix/pull/931/files) `Nix.Expr.Types`: Fix CPP on `Instances.TH.Lift` import.

## [(diff)](https://github.com/haskell-nix/hnix/compare/0.12.0...0.13.0#files_bucket) 0.13.0 (2021-05-10)

* Breaking:

  * [(link)](https://github.com/haskell-nix/hnix/pull/859/files) [(link)](https://github.com/haskell-nix/hnix/pull/863/files) [(link)](https://github.com/haskell-nix/hnix/pull/866/files) `Nix.Thunk`: `class MonadThunk t m a | t -> m, t -> a`. Class was initially designed with Kleisli arrows (`v -> m a`) in mind, which where put to have the design open and inviting customization & integration. Those functional arguments are for custom implementation, so  which in reality of the project were never used and HNax just "essentially" (simplifying, because `pure` was mixed with monadic binds to `f`) was passing `pure` into them (actually, `f <=< pure`). These Kliesli functors got arguments sorted properly and were moved to a `MonadThunkF` class and names gained `*F`. And `MonadThunk` now does only what is needed, for example `force` gets the thunk and computes it. All `MonadThunk{,F}` functions become with a classic Haskell arguments order, specialized, and got more straigh-forward to understand and use, and so now they tail recurse also.
      
      Now, for example, instead of `force t f` use it as `v <- force t` `f =<< force t`, or `f <=< force`.
      
      tl;dr: results:
      
      ```haskell
      class MonadThunkId m => MonadThunk t m a | t -> m, t -> a where
      
        thunkId  :: t -> ThunkId m
      
        thunk    :: m a -> m t
      
        queryM   :: m a -> t -> m a
        -- old became `queryMF`
      
        force    :: t -> m a
        -- old became `forceF`
      
        forceEff :: t -> m a
        -- old became `forceEffF`
      
        further  :: t -> m t
        -- old became `furtherF`
      
      
      -- | Class of Kleisli functors for easiness of customized implementation developlemnt.
      class MonadThunkF t m a | t -> m, t -> a where

        queryMF   :: (a   -> m r) -> m r -> t -> m r
        -- was :: t -> m r -> (a   -> m r) -> m r

        forceF    :: (a   -> m r) -> t   -> m r
        -- was :: t   -> (a   -> m r) -> m r

        forceEffF :: (a   -> m r) -> t   -> m r
        -- was :: t   -> (a   -> m r) -> m r

        furtherF  :: (m a -> m a) -> t   -> m t
        -- was :: t   -> (m a -> m a) -> m t

      ```
      
  * [(link)](https://github.com/haskell-nix/hnix/pull/862/files) [(link)](https://github.com/haskell-nix/hnix/pull/870/files) [(link)](https://github.com/haskell-nix/hnix/pull/871/files)  [(link)](https://github.com/haskell-nix/hnix/pull/872/files) [(link)](https://github.com/haskell-nix/hnix/pull/873/files) `Nix.Value.Monad`: `class MonadValue v m`: instances became specialized, Kleisli versions unflipped the arguments of methods into a classical order and moved to the `class MonadValueF`. As a result, `demand` now gets optimized by GHC and also tail recurse. Please, use `f =<< demand t`, or just use `demandF`, while `demandF` in fact just `kleisli =<< demand t`.
      
      ```haskell
      class MonadValue v m where
      
        demand :: v       ->         m v
        -- old became `demandF`

        inform :: v -> m v
        -- old became `informF`
      

      class MonadValueF v m where

        demandF :: (v -> m r) -> v -> m r
        -- was :: v -> (v -> m r) -> m r

        informF :: (m v -> m v) -> v -> m v
        -- was :: v -> (m v -> m v) -> m v
      ```
      
  * [(link)](https://github.com/haskell-nix/hnix/pull/863/files) `Nix.Normal`: `normalizeValue` removed first functional argument that was passing the function that did the thunk forcing. Now function provides the thunk forcing. Now to normalize simply use `normalizeValue v`. Old implementation now is `normalizeValueF`.

  * [(link)](https://github.com/haskell-nix/hnix/pull/859/commits/8e043bcbda13ea4fd66d3eefd6da690bb3923edd) `Nix.Value.Equal`: `valueEqM`: freed from `RankNTypes: forall t f m .`.

  * [(link)](https://github.com/haskell-nix/hnix/pull/802/commits/529095deaf6bc6b102fe5a3ac7baccfbb8852e49#) `Nix.Strings`: all `hacky*` functions replaced with lawful implemetations, because of that all functions become lawful - dropped the `principled` suffix from functions:
    * `Nix.String`:
        ```haskell
        hackyGetStringNoContext          ->
             getStringNoContext
        hackyStringIgnoreContext         ->
             stringIgnoreContext
        hackyMakeNixStringWithoutContext ->
             makeNixStringWithoutContext

        principledMempty        -> mempty
        principledStringMempty  -> mempty
        principledStringMConcat -> mconcat
        principledStringMappend -> mappend

        principledGetContext                        ->
                  getContext
        principledMakeNixString                     ->
                  makeNixString
        principledIntercalateNixStrin               ->
                  intercalateNixString
        principledGetStringNoContext                ->
                  getStringNoContext
        principledStringIgnoreContext               ->
                  stringIgnoreContext
        principledMakeNixStringWithoutContext       ->
                  makeNixStringWithoutContext
        principledMakeNixStringWithSingletonContext ->
                  makeNixStringWithSingletonContext
        principledModifyNixContents                 ->
                  modifyNixContents
        ```

  * [(link)](https://github.com/haskell-nix/hnix/pull/805/files):
    * Data type: `MonadFix1T t m`: `Nix.Standard` -> `Nix.Utils.Fix1`
    * Children found their parents:
        
        ```haskell
        Binary   NAtom :: Nix.Expr.Types -> Nix.Atoms
        FromJSON NAtom :: Nix.Expr.Types -> Nix.Atoms
        ToJSON   NAtom :: Nix.Expr.Types -> Nix.Atoms

        Eq1 (NValueF p m)     :: Nix.Value.Equal -> Nix.Value

        Eq1 (NValue' t f m a) :: Nix.Value.Equal -> Nix.Value 

        HasCitations m v (NValue' t f m a) :: Nix.Pretty -> Nix.Cited
        HasCitations m v (NValue  t f m)   :: Nix.Pretty -> Nix.Cited

        when
          (package hashable >= 1.3.1) -- gained instance
          $ Hashable1 NonEmpty:: Nix.Expr.Types -> Void -- please use upstreamed instance

        -- | Was upstreamed, released in `ref-tf >= 0.5`.
        MonadAtomicRef   (ST s) :: Nix.Standard -> Void

        MonadAtomicRef   (Fix1T t m) :: Nix.Standard -> Nix.Utils.Fix1
        MonadRef         (Fix1T t m) :: Nix.Standard -> Nix.Utils.Fix1
        MonadEnv         (Fix1T t m) :: Nix.Standard -> Nix.Effects
        MonadExec        (Fix1T t m) :: Nix.Standard -> Nix.Effects
        MonadHttp        (Fix1T t m) :: Nix.Standard -> Nix.Effects
        MonadInstantiate (Fix1T t m) :: Nix.Standard -> Nix.Effects
        MonadIntrospect  (Fix1T t m) :: Nix.Standard -> Nix.Effects
        MonadPaths       (Fix1T t m) :: Nix.Standard -> Nix.Effects
        MonadPutStr      (Fix1T t m) :: Nix.Standard -> Nix.Effects
        MonadStore       (Fix1T t m) :: Nix.Standard -> Nix.Effects
        MonadFile        (Fix1T t m) :: Nix.Standard -> Nix.Render

        MonadEnv         (Fix1 t)    :: Nix.Standard -> Nix.Effects
        MonadExec        (Fix1 t)    :: Nix.Standard -> Nix.Effects
        MonadHttp        (Fix1 t)    :: Nix.Standard -> Nix.Effects
        MonadInstantiate (Fix1 t)    :: Nix.Standard -> Nix.Effects
        MonadIntrospect  (Fix1 t)    :: Nix.Standard -> Nix.Effects
        MonadPaths       (Fix1 t)    :: Nix.Standard -> Nix.Effects
        MonadPutStr      (Fix1 t)    :: Nix.Standard -> Nix.Effects
        ```
  * [(link)](https://github.com/haskell-nix/hnix/pull/878/files) `Nix.Value`: `nvSet{,',P}`: got unflipped, now accept source position argument before the value.
  
  * [(link)](https://github.com/haskell-nix/hnix/pull/878/files) `Nix.Pretty`: `mkNixDoc`: got unflipped.
  
  * [(link)](https://github.com/haskell-nix/hnix/pull/886/commits/381b0e5df9cc620a25533ff1c84045a4ea37a833) `Nix.Value`: Data constructor for `NValue' t f m a` changed (`NValue -> NValue'`).
  
  * [(link)](https://github.com/haskell-nix/hnix/pull/884/files) `Nix.Parser`: `Parser`: Data type was equivalent to `Either`, so became a type synonim for `Either`.

  * [(link)](https://github.com/haskell-nix/hnix/pull/884/files) `Nix.Thunk.Basic`: `instance MonadThunk (NThunkF m v) m v`: `queryM`: implementation no longer blocks the thunk resource it only reads from.
  
  * [(link)](https://github.com/haskell-nix/hnix/pull/908/files): Migrated `(String -> Text)`:
    * `Nix.Value`: `{NValueF, nvBuiltin{,'}, builtin{,2,3}, describeValue}`
    * `Nix.Eval`: `MonadNixEval`
    * `Nix.Render.Frame`: `render{Expr,Value}`
    * `Nix.Type`: `TVar`
    * `Nix.Thunk`: `ThunkLoop`
    * `Nix.Exec`: `{nvBuiltinP, nixInstantiateExpr, exec}`
    * `Nix.Effects`:
      * `class`:
        * `MonadExec: exec'`
        * `MonadEnv: getEnvVar`
        * `MonadInstantiate: instatiateExpr`
      * `parseStoreResult`
    * `Nix.Effects.Derivation`: `renderSymbolic`
    * `Nix.Lint`: `{NTypeF, symerr}`

* Additional:
  * [(link)](https://github.com/haskell-nix/hnix/commit/7e6cd97bf3288cb584241611fdb25bf85d7e0ba7) `cabal.project`: freed from the `cryptohash-sha512` override, Hackage trustees made a revision.
  * [(link)](https://github.com/haskell-nix/hnix/pull/824/commits/4422eb10959115f21045f39e302314a77df4b775) To be more approachable for user understanding, the thunk representation in outputs changed from `"<CYCLE>" -> "<expr>"`.
  * [(link)](https://github.com/haskell-nix/hnix/pull/925/commits/37e81c96996b07cbbdf9fa4bf380265e8c008482) The Nix evaluation cycle representation changed `"<CYCLE>" -> "<cycle>"`.
  * [(link)](https://github.com/haskell-nix/hnix/commit/51a3ff9e0065d50e5c625738696526c4a232b0cf) `Nix.Expr.Types`: added hacky implementation of `liftTyped` for `instance Lift (Fix NExprF)`.
  * [(link)](https://github.com/haskell-nix/hnix/commit/51a3ff9e0065d50e5c625738696526c4a232b0cf) `Nix.Builtins`: `derivation` primOp internal code always fully evaluated, so GHC now always ships only fully compiled version in the bytecode.
  * [(link)](https://github.com/haskell-nix/hnix/pull/890): Project switched to prelude `relude`.
  * A bunch of other stuff that is not user-facing.


## [(diff)](https://github.com/haskell-nix/hnix/compare/0.11.1...0.12.0#files_bucket) 0.12.0 (2021-01-05)

* *Disclaimer*: Current `derivationStrict` primOp implementation and so every evaluation of a derivation into a store path currently relies on the `hnix-store-remote`, which for those operations relies on the running `nix-daemon`, and so operations use/produce effects into the `/nix/store`. Be cautious - it is effectful.

* Introduction:
  * New module `Nix.Effects.Derivation`.
  * Operations on derivations:
    * old got principled implementations.
    * also new operations got introduced.
  * Implementation of the `derivationStrict` primOp.

* Breaking:
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/commits/3bba5549273c892c60aad5dd6d5058a8db40efbf) `Nix.Effects`: **class** `MonadStore` got principled implementation.
    * `addPath'` got principled into `addToStore`.
    * `toFile_` got principled into `addTextToStore'`.
    * For help & easy migration you may use `addPath` & `toFile_` `addTextToStore` standalone functions in the module.
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/commits/3bba5549273c892c60aad5dd6d5058a8db40efbf) `Nix.Effects.Basic`: `defaultDerivationStrict` got reimplemented & moved into `Nix.Effects.Derivation`.
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/commits/3bba5549273c892c60aad5dd6d5058a8db40efbf) `Nix.Standard`: instance for `MonadStore (Fix1T t m)` got principled accoding to class `MonadStore` changes.
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/commits/3bba5549273c892c60aad5dd6d5058a8db40efbf) `Nix.Fresh.Basic`: instance for `MonadStore (StdIdT m)` got principled.

* Additional:
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/commits/3bba5549273c892c60aad5dd6d5058a8db40efbf) **New module `Nix.Effects.Derivation`**: [HNix(0.12.0):Nix.Effects.Derivation documentation](https://hackage.haskell.org/package/hnix-0.12.0/docs/Nix-Effects-Derivation.html).
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/commits/9bcfbbe88ff0bd8d803296193ee1d8603dc5289e) `Nix.Convert`: Principled `NVPath -> NixString` coercion.
    * In a form of principled `instance FromValue NixString m (NValue' t f m (NValue t f m))`.
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/commits/a8e6d28fdb98a1c34f425c8395338fdabe96becc) `Nix.String`: Allow custom computations inside string contexts.
    * By providing `runWithStringContext{T,}'` methods into the API.
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/commits/e45f7632c51a9657f6e8d54c39fd4d21c466d85f) Includded support for new `base16-bytestring`, which advertices 2x-4x speed increase of its operations.
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/commits/3bba5549273c892c60aad5dd6d5058a8db40efbf) `Nix.Effects`: `addPath` & `toFile_` standalone functions got principled implementation through the internal use of the new `MonadStore` type class implementation.
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/commits/3bba5549273c892c60aad5dd6d5058a8db40efbf) `Nix.Effects`: added `addTextToStore`, `parseStoreResult` implementations.
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/commits/3bba5549273c892c60aad5dd6d5058a8db40efbf) `Nix.Effects`: added type synonyms `{RecursiveFlag, RepairFlag, StorePathName, FilePathFilter, StorePathSet}`.
  * [(link)](https://github.com/haskell-nix/hnix/pull/760) `Nix.Exec`: Fixed the rendering of internal `Frames`.
    * Which is an internal mechanism of a project to passing around messages with their context, still to be used internally).
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/commits/3bba5549273c892c60aad5dd6d5058a8db40efbf) `HNix / Nix`: The library now also uses `hnix-store-remote`.
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/commits/06b0fca9fd607eb2e995f003424e797a41ffa5b7) `cabal.project`: project uses `cryptohash-sha512` override, the `hnix-store-core` requires it from `hnix` and uses that override also. [Detailed info](https://github.com/haskell-hvr/cryptohash-sha512/pull/5#issuecomment-752796913). We promise to attend to this issue, probably by migrating to `cryptonite` in the nearest releases.

Future note: The HNix is a big project. During the initial development and currently the API for simplicity exposes allmost all functions, types, etc. Big open API means a big effort to create/maintain a quite noisy changelog and you parsing through it, and also creates a frequent requirements to mark releases as major and bother you due to some type changes in some parts that may not be used or applicable to be public API.

This year the most gracious API clean-up would happen, we would check and keep open what Hackage projects are using from the API, and the other parts would be open on the request by a way of rapid minor releases. That clean-up is also a work toward splitting the project into several packages some time in the future (split would be into something like Expressions, Evaluation, Executable, Extra), which migration also would be done most thoughful and graceful as possible, with as much easiness and automation provided for migration downstream as possible. If all goes as planned - all downstream would need to do is to get and run our script that would migrate our old map of module imports to new ones, and maybe manually add those newly formed packages into `.cabal` description.

If it is possible, please, try to switch & use the higher-level API functions where it is applicable. Thank you.


### [(diff)](https://github.com/haskell-nix/hnix/compare/0.11.0...0.11.1#files_bucket) 0.11.1 (2020-12-09)

* Additional:
  * [(link)](https://github.com/haskell-nix/hnix/commit/d32a6fbaf3df1c8879d1b19a18f21c031a73e56c) `Nix.Builtins`: `isString` fixed - It used to return `True` for values coercible to string like derivations and paths. It only accepts string values now.
  * [(link)](https://github.com/haskell-nix/hnix/commit/53b4db2525a8f074d8c262fa7b66ce97e5820890) `Nix.Builtins`: `substring` fixed - Negative lengths used to capture an empty string. Now they capture the whole rmeainder of the string.
  * [(link)](https://github.com/haskell-nix/hnix/commit/dc31c5e64f8c7aaaea14cac0134bd47544533e67) `Nix.Effects`: `pathExists` fixed - Now also works with directories.
  * [(link)](https://github.com/haskell-nix/hnix/commit/e2ad934492eeac9881527610e4a1c1cf31ea1115) `Nix.Parser`: `->` is now properly right-associative (was non-associative).
  * [(link)](https://github.com/haskell-nix/hnix/commit/50baea5e1e482be3c4fcc13c9a45b1083243f681) `Nix.Parser`: Nix `assert` parser (`nixAssert` function) now accepts top-level Nix format also (which means also accepts all kinds of statements), before that it accepted only regular Nix expressions.
  * [(link)](https://github.com/haskell-nix/hnix/commit/59698de7185dfae508e5ccea4377a82023c4a0d5) `Nix.Render`: `renderLocation` now also shows/handles location of errors in raw strings.


## [(diff)](https://github.com/haskell-nix/hnix/compare/0.10.1...0.11.0#files_bucket) 0.11.0 (2020-11-02)

* Breaking:
  * [(link)](https://github.com/haskell-nix/hnix/pull/740) Deleted incorrect `instance Generic1 NKeyName` from `module Nix.Expr.Types`.
  * [(link)](https://github.com/haskell-nix/hnix/pull/739) Parentheses now are properly included in the location annotation for Nix expressions, change of `nixParens` in `module Nix.Parser` essentially results in the change of all module `nix*` function results, essentially making results of the whole module more proper.

* Additional:
  * [(link)](https://github.com/haskell-nix/hnix/pull/741) Fix QQ Text lifting error: work around of [GHC#12596 "can't find interface-file declaration"](https://gitlab.haskell.org/ghc/ghc/-/issues/12596).
  * [(link)](https://github.com/haskell-nix/hnix/pull/744) Fix comments inclusion into location annotations, by using pre-whitespace position for source end locations.


### [(diff)](https://github.com/haskell-nix/hnix/compare/0.10.0...0.10.1#files_bucket) 0.10.1 (2020-09-13)

* Additional:
  * [(link)](https://github.com/haskell-nix/hnix/pull/715) `{Binding, NExpr, NExprF, NKeyName}` gained `Ord1` instances.
    * These instances were required by downstream projects to be able to use newer HNix.
  * [(link)](https://github.com/haskell-nix/hnix/pull/712) CLI gained `--long-version` option for gathering a detailed debug information.
    * Currently, reports Git commit and its date.
    * [(link)](https://github.com/haskell-nix/hnix/issues/718) Currently does not work in case of use of the `nix-build`, in which case simply returns `UNKNOWN` placeholder.


## [(diff)](https://github.com/haskell-nix/hnix/compare/0.9.1...0.10.0#files_bucket) 0.10.0 (2020-09-12)

* Breaking:
  * [(link)](https://github.com/haskell-nix/hnix/pull/699) Removed `NExpr` `{FromJSON, ToJSON}` instances.
    * This also removed the JSON output feature for unevaluated expression trees.

* Additional:
  * [(link)](https://github.com/haskell-nix/hnix/pull/703) CLI gained `--version` option.
  * Dependencies:
    * [(link)](https://github.com/haskell-nix/hnix/pull/686) Requires last major `data-fix` (`0.3`).
    * [(link)](https://github.com/haskell-nix/hnix/pull/679) Requires last major `prettyprinter` (`1.7`).


### [(diff)](https://github.com/haskell-nix/hnix/compare/0.9.0...0.9.1#files_bucket) 0.9.1 (2020-07-13)

* Additional:
  * REPL:
    * Better tab completion.
    * Accepting multi-line input.
    * Support for passing evaluated expression result of `hnix --eval -E`.
      to REPL as an `input` variable.
    * Support for loading `.hnixrc` from the current directory.
  * Reporting of `builtins.nixVersion` bumped from 2.0 to 2.3.
  * Dependencies:
    * Freed from: `{interpolate, contravariant, semigroups, generic-random, tasty-quickcheck}`.
    * Requires last major `repline` (`0.4`).


## [(diff)](https://github.com/haskell-nix/hnix/compare/0.8.0...0.9.0#files_bucket) 0.9.0 (2020-06-15)

* Breaking:
  * Removed instances due to migration to `haskeline 0.8`:
    * `instance MonadException m => MonadException(StateT(HashMap FilePath NExprLoc) m)`.
    * `instance MonadException m => MonadException(Fix1T StandardTF m)`.
  * Dependencies:
    * Requires last major `haskeline` (`0.8`).

* Additional:
  * Library: Official support for `GHC 8.4 - 8.10`.
  * Executable complies only under `GHC 8.10`.

* Changelog started. Previous release was `0.8.0`.


---

HNix uses [PVP Versioning][1].

[1]: https://pvp.haskell.org
