[<AutoOpen>]
module CustomShapes
open System
open TypeShape.Core
    
type INullableComparisonVisitor<'R> =
    abstract member Visit<'t when 't : (new : unit -> 't)
                                and 't :> ValueType
                                and 't : struct
                                and 't : comparison> : unit -> 'R
        
type IShapeNullableComparison =
    abstract member Element : TypeShape
    abstract member Accept : INullableComparisonVisitor<'R> -> 'R
        
type private ShapeNullableComparison<'T when 'T : (new : unit -> 'T) 
                                            and 'T :> ValueType 
                                            and 'T : struct 
                                            and 'T : comparison>() =
    interface IShapeNullableComparison with
        override val Element = shapeof<'T> :> TypeShape
        override _.Accept (v: INullableComparisonVisitor<'R>) = v.Visit<'T>()
    
[<RequireQualifiedAccess>]
module Shape =
    let (|NullableComparison|_|) (s: TypeShape) =
        match s with
        | Shape.Nullable s ->
            match s.Element with
            | Shape.Comparison _ & s ->
                Activator.CreateInstanceGeneric<ShapeNullableComparison<_>> [|s.Type|]
                :?> IShapeNullableComparison
                |> Some
    
            | _ -> None
    
        | _ -> None
