(**
 - title: WebGL Geometry Terrain
 - tagline: A 3D world right in the browser
 - app-style: height:450px; width:800px; margin:20px auto 50px auto;
 - intro: This demo is a Fable port of the [WebGL Geometry Terrain](http://threejs.org/examples/#webgl_geometry_terrain)
   three.js demo. It uses the three.js library to randomly generate a 3D terrain which can be navigated in a first-person view.
   The code was originally written by [John Quigley](https://github.com/jmquigs) for FunScript,
   you can find [Fable's version on GitHub](https://github.com/fable-compiler/Fable/blob/master/samples/browser/webGLTerrain/webGLTerrain.fsx).

   On the technical side, the demo shows some of the more interesting aspects of
   calling JavaScript libraries from Fable. You'll learn how to define mapping for
   global objects and other useful functions.
*)
(*** hide ***)
// #r "../../node_modules/fable-core/Fable.Core.dll"
// #load "../../node_modules/fable-import-three/Fable.Import.Three.fs"
(**
JavaScript helpers and imports
------------------------------

Fable comes with [an F# mapping for three.js](https://github.com/fable-compiler/Fable/tree/master/import/three),
which defines all the types and functions for three.js that we'll need in this example.
In addition this demo uses custom scripts for ImprovedNoise and FirstPersonControls.
We'll write the mappings for those two inline.
*)

module WebGL.Terrain

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

/// ------------ Units of Measure ---------------
[<Measure>] type centimeters
[<Measure>] type kilograms
[<Measure>] type degrees

/// Represents the API exposed by ImprovedNoise script
type IPerlin =
    abstract noise: x:float * y:float * z:float -> float

/// Represents the API exposed by FirstPersonControls script
type IFirstPersonControls =
    abstract movementSpeed: float with get, set
    abstract lookSpeed: float with get, set
    abstract handleResize: unit -> unit
    abstract update: float -> unit

type IOrbitControls =
    abstract handleResize: unit -> unit
    abstract update: unit -> unit

let ImprovedNoise: unit->IPerlin = importDefault "./lib/ImprovedNoise.js"
let FirstPersonControls: JsConstructor<Three.Camera, Browser.HTMLElement, IFirstPersonControls> =
    importDefault "./lib/FirstPersonControls.js"

let OrbitControls: JsConstructor<Three.Camera, Browser.HTMLElement, IOrbitControls> =
    importDefault "./lib/OrbitControls.js"    

// Algorithm code  ....

//
// -------------   Model   --------------
//

type Details = 
  {
      Name: string
      Description: string
  }


type Dimensions =
  {
    Length: decimal<centimeters>
    Width: decimal<centimeters>
    Height: decimal<centimeters>
  }

 type ConvexHull =
  {
    Dimensions: Dimensions
  }

 type Point = 
  {
    X: decimal<centimeters>
    Y: decimal<centimeters>
    Z: decimal<centimeters>
  }

 type Origin =
  {
    Point : Point
  }

 type ExtremePoints = 
  {
    LeftFrontBottomPoint: Point
    RightBehindBottomPoint: Point
    LeftBehindTopPoint: Point
  }

type Orientation =
  {
      Phi: decimal<degrees>
      Theta: decimal<degrees>
  }

type Toy =
  {
      Details: Details
      Dimensions: Dimensions
      Weight: decimal<kilograms>
      Origin: Point
  }

type ToyWithExtremePoints =
  {
      Toy: Toy * ExtremePoints
  }

type ToyWithExtremePointsAndOrientations =
  {
      ToyWithExtremePoints: ToyWithExtremePoints * Orientation
  }

type OrderId =
   {
     OrderId: string
   }

type Order =
  {
      Details: Details
      OrderId: OrderId
      Toys: Toy list
  }

type Packing =
  {
      Order: Order
      SetOfExtremePoints: Point list
  }

type BoxId =
   {
     BoxId: string
   }

type Box =
  {
    Id: BoxId
    Details: Details
    Dimensions: Dimensions   
  }

type PackagedOrder = 
   | BoxedOrder of Order * Box

type Customer =
  {
      Details: Details
      Cart: Order option
  }

type World =
  {
      AvailableBoxes: Map<BoxId, Box>
      Customer: Customer
      Order: Order
  }

// ------------ Initial World --------------

let initialSetOfExtremePoints = 
  [
    {
        X = 0M<centimeters>
        Y = 0M<centimeters>
        Z = 0M<centimeters>
    }

  ]

let blankOrder =
  {
    Details =
      {
        Name = "";
        Description = ""
      }
    OrderId = 
      {
        OrderId = "";
      }
    Toys = 
      [
        {
          Details = 
            {
              Name = "";
              Description = ""
            }
          Dimensions = 
            {
              Length = 0M<centimeters>
              Width = 0M<centimeters>
              Height = 0M<centimeters>
            }
          Weight = 0M<kilograms>
          Origin =             
            { 
              X = 0M<centimeters>
              Y = 0M<centimeters>
              Z = 0M<centimeters>
            }
        }               
      ]
    }

let customerOrder =
  {
    Details =
      {
        Name = "Order#1Name";
        Description = "Order#1Description"
      }
    OrderId = 
      {
        OrderId = "Order#1";
      }
    Toys = 
      [
        {
          Details = 
            {
              Name = "Toy#1Name";
              Description = "Toy#1Description"
            }
          Dimensions = 
            {
              Length = 8M<centimeters>
              Width = 4M<centimeters>
              Height = 4M<centimeters>
            }
          Weight = 1M<kilograms>
          Origin =             
            { 
              X = 0M<centimeters>
              Y = 0M<centimeters>
              Z = 0M<centimeters>
            }
        }
        {
          Details = 
            {
              Name = "Toy#2Name";
              Description = "Toy#2Description"
            };                 
          Dimensions = 
            {
              Length = 8M<centimeters>
              Width = 4M<centimeters>
              Height = 4M<centimeters>
            }
          Weight = 2M<kilograms>
          Origin =             
            { 
              X = 0M<centimeters>
              Y = 0M<centimeters>
              Z = 0M<centimeters>
            }
        }
        {
          Details = 
            {
              Name = "Toy#3Name";
              Description = "Toy#3Description"
            };                 
          Dimensions = 
            {
              Length = 8M<centimeters>
              Width = 4M<centimeters>
              Height = 4M<centimeters>
            }
          Weight = 2M<kilograms>
          Origin =             
            { 
              X = 0M<centimeters>
              Y = 0M<centimeters>
              Z = 0M<centimeters>
            }
        } 
      ]
    }

let allBoxes = 
  [
    {
        Id = 
          {
              BoxId = "12";
          }
        Details =
          {
            Name = "Box12"
            Description = "Our very favorite box, Box #12."
          }
        Dimensions =
          {
             Length = 20M<centimeters>;
             Width = 15M<centimeters>;
             Height = 10M<centimeters>
          };
    }
    {
        Id = 
          {
              BoxId = "13";
          }
        Details =
          {
            Name = "Box13"
            Description = "Our second favorite box, Box #13."
          }          
        Dimensions =
          {
             Length = 35M<centimeters>;
             Width = 20M<centimeters>;
             Height = 30M<centimeters>
          };
    }    
  ]

let customer =
  {
    Details = 
      { 
        Name = "ValuedCustomer";
        Description = "Our very first customer."
      }
    Cart = None
  }

let gameWorld =
  {
    AvailableBoxes =
      allBoxes
      |> Seq.map (fun box -> (box.Id, box))
      |> Map.ofSeq
    Customer = customer
    Order = customerOrder       
  }

//
// -------------   Railway Oriented Programming   --------------
//

type Result<'TSucess, 'TFailure> =
  | Success of 'TSucess
  | Failure of 'TFailure

let bind processFunction lastResult =
  match lastResult with
  | Success s -> processFunction s
  | Failure f -> Failure f

let (>>=) x f =
  bind f x

let switch processFunction input =
  Success (processFunction input)

let displayResult result =
  match result with
  | Success s -> printfn "%s" s
  | Failure f -> printfn "%s" f

let getResult result =
  match result with
  | Success s -> s
  | Failure f -> f

//
// -------------   Logic   --------------
//

let getOrientation orientation =
  match (orientation.Theta, orientation.Phi) with
  |  (theta, phi) 
      when theta >= 0M<degrees> && theta <= 360M<degrees>
           && phi >= 0M<degrees> && theta <= 360M<degrees>
         -> Success orientation
  |  (_, _) -> Failure "Orientation does not exist"

let getMaxExtremePointsHeight (extremePoints:ExtremePoints) =
  let leftFrontRemoveMeasure = extremePoints.LeftFrontBottomPoint.Z / 1M<centimeters>
  let rightBehindRemoveMeasure = extremePoints.RightBehindBottomPoint.Z / 1M<centimeters>
  let leftBehindRemoveMeasure = extremePoints.LeftBehindTopPoint.Z / 1M<centimeters>
  let list = [leftFrontRemoveMeasure; rightBehindRemoveMeasure; leftBehindRemoveMeasure]
  let maxValueOfList = list |> List.max
  let maxWithMeasure = maxValueOfList * 1M<centimeters>
  maxWithMeasure

let getPackedToyConvexHullHeight packedToy =
  let packedToyConvexHullHeight = packedToy.Origin.Y + packedToy.Dimensions.Height
  packedToyConvexHullHeight

let getPackedToyConvexHullWidth packedToy =
  let packedToyConvexHullWidth = packedToy.Origin.X + packedToy.Dimensions.Width
  packedToyConvexHullWidth

let calculateConvexHull packedToy maxHeightSoFar = 
      let toyHeight = getPackedToyConvexHullHeight(packedToy)
      let list = [toyHeight; maxHeightSoFar]
      let maxValueOfList = list |> List.max
      maxValueOfList

let rec getHeightOfConvexHullOfCurrentlyPackedToys packedToys  maxHeightSoFar =
  match packedToys with
  | head :: tail ->
      let maxHeightWithMeasure = calculateConvexHull head maxHeightSoFar
      getHeightOfConvexHullOfCurrentlyPackedToys tail maxHeightWithMeasure
  | [] -> maxHeightSoFar

let getHeightOfConvexHullOfCurrentPacking (packedToys:Packing, maxHeightSoFar:decimal<centimeters>) =
  match packedToys.Order.Toys with
  | head :: tail ->
      let maxHeightWithMeasure = calculateConvexHull head maxHeightSoFar
      getHeightOfConvexHullOfCurrentlyPackedToys tail maxHeightWithMeasure
  | [] -> maxHeightSoFar

let volume length width height = 
  length * width * height

let rec getToysVolume (toys:Toy list, accumulator) =
  match toys with
  | head :: tail ->
      let toyVolumePacking = volume head.Dimensions.Length head.Dimensions.Width head.Dimensions.Height
      getToysVolume(tail, (accumulator + toyVolumePacking))
  | [] -> accumulator

let calculateConvexHullWithCurrentToy(extremePoint:Point, toy:Toy, convexHull:ConvexHull) =
  let toyLength = extremePoint.X + toy.Dimensions.Length
  let listLength = [toyLength; convexHull.Dimensions.Length]
  let maxValueOfListLength = listLength |> List.max

  let toyWidth = extremePoint.Y + toy.Dimensions.Width
  let listWidth = [toyWidth; convexHull.Dimensions.Width]
  let maxValueOfListWidth = listWidth |> List.max

  let toyHeight = extremePoint.Z + toy.Dimensions.Height
  let listHeight = [toyHeight; convexHull.Dimensions.Height]
  let maxValueOfListHeight = listHeight |> List.max

  let newConvexHull =
    {
      Dimensions = 
        {
          Length = maxValueOfListLength
          Width = maxValueOfListWidth
          Height = maxValueOfListHeight
        }
    }
  newConvexHull

let convexHullLength(extremePoint:Point, toy:Toy, currentConvexHullLengthMax) =
  let toyLength = extremePoint.X + toy.Dimensions.Length
  let list = [toyLength; currentConvexHullLengthMax]
  let maxValueOfList = list |> List.max
  maxValueOfList

let convexHullWidth(extremePoint:Point, toy:Toy, currentConvexHullWidthMax) =
  let toyWidth = extremePoint.Y + toy.Dimensions.Width
  let list = [toyWidth; currentConvexHullWidthMax]
  let maxValueOfList = list |> List.max
  maxValueOfList

let convexHullHeight(extremePoint:Point, toy:Toy, currentConvexHullHeightMax) =
  let toyHeight = extremePoint.Z + toy.Dimensions.Height
  let list = [toyHeight; currentConvexHullHeightMax]
  let maxValueOfList = list |> List.max
  maxValueOfList

let tolerance = 0.001M<centimeters>

let roomInBoxForNewToy (dimensions:Dimensions, toy:Toy) =
  let lengthAvailability = dimensions.Length - (toy.Origin.X + toy.Dimensions.Length) >= tolerance
  let widthAvailability = dimensions.Width - (toy.Origin.Y + toy.Dimensions.Width) >= tolerance
  let heightAvailability = dimensions.Height - (toy.Origin.Z + toy.Dimensions.Height) >= tolerance
  let temp = heightAvailability
  lengthAvailability && widthAvailability && heightAvailability

let attemptingToDivideByZero volumeOfToysPackedSoFarIncludingCurrentToy =
  volumeOfToysPackedSoFarIncludingCurrentToy = 0M<centimeters^3>

/// Measure of height over the fill rate of the current convex hull
let packingIndex(length:decimal<centimeters>, width:decimal<centimeters>, height:decimal<centimeters>, volumeOfToysPackedSoFarIncludingCurrentToy:decimal<centimeters^3>) =
  let heightSquared = height * height
  let numerator = length * width * heightSquared
  let index =
    if (attemptingToDivideByZero volumeOfToysPackedSoFarIncludingCurrentToy) then
      numerator/(1M * 1M<centimeters^3>)
    else
      (numerator/volumeOfToysPackedSoFarIncludingCurrentToy)
  index

let packingIndexZ(length:decimal<centimeters>, width:decimal<centimeters>, height:decimal<centimeters>, volumeOfToysPackedSoFarIncludingCurrentToy:decimal<centimeters^3>, lowestZRatio:decimal<centimeters>, roomRatio:decimal<centimeters>) =
  let heightDifferential = height * lowestZRatio
  let numerator = length * width * heightDifferential * roomRatio

  let index =
    if (attemptingToDivideByZero volumeOfToysPackedSoFarIncludingCurrentToy) then
      numerator/(1M * 1M<centimeters^3>)
    else
      (numerator/volumeOfToysPackedSoFarIncludingCurrentToy)
  index

let maxPackingIndex x y = 
  let (a, b, c) = x
  let (d, e, f) = y
  if a < d then (d, e, f) else (a, b, c)

let rec calculateIndex (currentToy:Toy, extremePoint:Point, volume:decimal<centimeters^3>, convexHull:ConvexHull, maxSoFar) =
  let convexHullPoint = 
    calculateConvexHullWithCurrentToy(extremePoint, currentToy, convexHull)
  let lpj = convexHullPoint.Dimensions.Length
  let wpj = convexHullPoint.Dimensions.Width
  let hpj = convexHullPoint.Dimensions.Height
  let index = (packingIndex(lpj, wpj, hpj, volume), extremePoint, convexHullPoint)

  let max = maxPackingIndex index maxSoFar
  max

let rec calculateIndexZValue (currentToy:Toy, box:Box, extremePoint:Point, volume:decimal<centimeters^3>, convexHull:ConvexHull, maxSoFar, lowestZRatio) =
  let convexHullPoint = 
    calculateConvexHullWithCurrentToy(extremePoint, currentToy, convexHull)
  let lpj = convexHullPoint.Dimensions.Length
  let wpj = convexHullPoint.Dimensions.Width
  let hpj = convexHullPoint.Dimensions.Height
  let newLowestZDifferential = 1M<centimeters> - max (extremePoint.Z - lowestZRatio) 0.1M<centimeters>
  let currentToyAtExtremePoint = 
        {
          Details = currentToy.Details
          Dimensions = currentToy.Dimensions
          Weight = currentToy.Weight
          Origin =             
            { 
              X = extremePoint.X
              Y = extremePoint.Y
              Z = extremePoint.Z
            }
        }
  
  let roomAvailable =  roomInBoxForNewToy (box.Dimensions, currentToyAtExtremePoint)
  let roomRatio =
    if roomAvailable then
      1M<centimeters>
    else
      0M<centimeters>    
  let index = (packingIndexZ(lpj, wpj, hpj, volume, newLowestZDifferential, roomRatio), extremePoint, convexHullPoint)

  let max = maxPackingIndex index maxSoFar
  max

let rec calculateIndexViaListOfExtremePointsRecursive (currentToy:Toy, extremePoints:Point list, volume:decimal<centimeters^3>, maxIndexSoFar, convexHull:ConvexHull) =
  match extremePoints with
  | head :: tail ->
      let extremePointsIndex = calculateIndex(currentToy, head, volume, convexHull, maxIndexSoFar) 
      calculateIndexViaListOfExtremePointsRecursive(currentToy, tail, volume, extremePointsIndex, convexHull)
  | [] -> maxIndexSoFar

let rec calculateIndexViaListOfExtremePointsZValuesRecursive (box:Box, currentToy:Toy, extremePoints:Point list, volume:decimal<centimeters^3>, maxIndexSoFar, convexHull:ConvexHull, lowestZValue:decimal<centimeters>) =
  match extremePoints with
  | head :: tail ->
      let extremePointsIndex = calculateIndexZValue(currentToy, box, head, volume, convexHull, maxIndexSoFar, lowestZValue) 
      calculateIndexViaListOfExtremePointsZValuesRecursive(box, currentToy, tail, volume, extremePointsIndex, convexHull, lowestZValue)
  | [] -> maxIndexSoFar

let sortMethodForExtremePointZValues (point1:Point) (point2:Point)=
  if (point1.Z > point2.Z) then
    1
  else if (point1.Z < point2.Z) then
    -1
  else
    if (point1.Z = point2.Z) then
      0
    else
      -1

let  calculateIndexViaListOfExtremePointsZValues (currentToy:Toy, box:Box, setOfExtremePoints:Point list, volume:decimal<centimeters^3>, maxIndexSoFar, convexHull:ConvexHull) =
  let sortedSetOfExtremePoints = (setOfExtremePoints |> List.sortWith(sortMethodForExtremePointZValues))
  let lowestZValue = sortedSetOfExtremePoints.[0].Z
  match sortedSetOfExtremePoints with
  | head :: tail ->
      let extremePointsIndex = calculateIndexZValue(currentToy, box, head, volume, convexHull, maxIndexSoFar, lowestZValue)
      calculateIndexViaListOfExtremePointsZValuesRecursive(box, currentToy, tail, volume, extremePointsIndex, convexHull, lowestZValue)
  | [] -> maxIndexSoFar

let  calculateIndexViaListOfExtremePoints (currentToy:Toy, setOfExtremePoints:Point list, volume:decimal<centimeters^3>, maxIndexSoFar, convexHull:ConvexHull) =
  match setOfExtremePoints with
  | head :: tail ->
      let extremePointsIndex = calculateIndex(currentToy, head, volume, convexHull, maxIndexSoFar)
      calculateIndexViaListOfExtremePointsRecursive(currentToy, tail, volume, extremePointsIndex, convexHull)
  | [] -> maxIndexSoFar

let calculateToyExtremePoints(currentToy: Toy) =
  let setOfExtremePoints = 
    [
      { 
        X = currentToy.Origin.X + currentToy.Dimensions.Length
        Y = currentToy.Origin.Y
        Z =  currentToy.Origin.Z
      }
      { 
        X = currentToy.Origin.X
        Y = currentToy.Origin.Y + currentToy.Dimensions.Width
        Z = currentToy.Origin.Z
      }
      { 
        X = currentToy.Origin.X
        Y = currentToy.Origin.Y
        Z = currentToy.Origin.Z + currentToy.Dimensions.Height
      }

    ]
  setOfExtremePoints

let revealDimensions dimensions =
  dimensions

let extractDimensionsFromBox (box : Box) =
  box.Dimensions

let noVolumeLeft remainingVolume =
  remainingVolume <= 0M<centimeters^3>

let blankPacking =
  {       
    SetOfExtremePoints = initialSetOfExtremePoints
    Order = blankOrder
  }

let doesBoxHaveEnoughFreeVolumeForToy (dimensions:Dimensions, toy:Toy, packing:Packing) =
  let toyVolume = volume toy.Dimensions.Length toy.Dimensions.Width toy.Dimensions.Height
  let boxVolume = volume dimensions.Length dimensions.Width dimensions.Height  
  let volumeLeft = boxVolume - toyVolume
  match volumeLeft with 
  |  i when noVolumeLeft i -> blankPacking
  |  _ -> packing

let rec fitnessEvaluation (toys:Toy list, box:Box, packing:Packing, convexHull:ConvexHull, maxIndexSoFar) =
  let accumulator = 0M<centimeters^3>
  match toys with
  | head :: tail ->
      let currentHeightOfConvexHull = getHeightOfConvexHullOfCurrentPacking(packing, convexHull.Dimensions.Height)
      let packedToyVolume = getToysVolume(packing.Order.Toys, accumulator)
      let currentToyVolume = volume head.Dimensions.Length head.Dimensions.Width head.Dimensions.Height
      let inclusiveToyVolume = packedToyVolume + currentToyVolume
      let index = calculateIndexViaListOfExtremePoints(head, packing.SetOfExtremePoints, inclusiveToyVolume, maxIndexSoFar, convexHull)
      let fitVolumePacking = doesBoxHaveEnoughFreeVolumeForToy(box.Dimensions, head, packing)
      let (_, indexPoint, _) = index
      let newConvexHull = calculateConvexHullWithCurrentToy(indexPoint, head, convexHull)
      let setOfExtremePointsFound = 
        [
          {
            X = indexPoint.X
            Y = indexPoint.Y
            Z = indexPoint.Z
          }
        ]
      let filteredExtremePoints = (Set fitVolumePacking.SetOfExtremePoints) - (Set setOfExtremePointsFound) |> Set.toList
      let newPackedToy =
        {
          Details = head.Details
          Dimensions = head.Dimensions
          Weight = head.Weight
          Origin = indexPoint
        }
      let availability = roomInBoxForNewToy(box.Dimensions, newPackedToy)
      let newExtremePointsFromNewlyPackedToy = calculateToyExtremePoints(newPackedToy)
      let newExtremePoints = (Set filteredExtremePoints) + (Set newExtremePointsFromNewlyPackedToy) |> Set.toList
      let newOrder =
        {
          Details = fitVolumePacking.Order.Details
          OrderId = fitVolumePacking.Order.OrderId
          Toys = newPackedToy::fitVolumePacking.Order.Toys      
        }
      let newPacking =
        {       
          SetOfExtremePoints = newExtremePoints
          Order = newOrder
        }
      fitnessEvaluation(tail, box, newPacking, newConvexHull, index)
  | [] -> packing

let rec packOrderInBoxRecursive (box:Box, toys:Toy list, packing:Packing, convexHull:ConvexHull, maxIndexSoFar) =
  let accumulator = 0M<centimeters^3>
  match toys with
  | head :: tail ->
    let currentToyVolume = volume head.Dimensions.Length head.Dimensions.Width head.Dimensions.Height
    let packedToyVolume = getToysVolume(packing.Order.Toys, accumulator)
    let inclusiveToyVolume = packedToyVolume + currentToyVolume
    let index = calculateIndexViaListOfExtremePointsZValues(head, box, packing.SetOfExtremePoints, inclusiveToyVolume, maxIndexSoFar, convexHull)
    let fitVolumePacking = doesBoxHaveEnoughFreeVolumeForToy(box.Dimensions, head, packing)
    let (_, indexPoint, _) = index
    let newConvexHull = calculateConvexHullWithCurrentToy(indexPoint, head, convexHull)
    let setOfExtremePointsFound = 
      [
        {
          X = indexPoint.X
          Y = indexPoint.Y
          Z = indexPoint.Z
        }
      ]
    let filteredExtremePoints = (Set fitVolumePacking.SetOfExtremePoints) - (Set setOfExtremePointsFound) |> Set.toList
    let newPackedToy =
      {
        Details = head.Details
        Dimensions = head.Dimensions
        Weight = head.Weight
        Origin = indexPoint
      }
    let availability = roomInBoxForNewToy(box.Dimensions, newPackedToy)
    let newExtremePointsFromNewlyPackedToy = calculateToyExtremePoints(newPackedToy)
    let newExtremePoints = (Set filteredExtremePoints) + (Set newExtremePointsFromNewlyPackedToy) |> Set.toList
    let newOrder =
      {
        Details = fitVolumePacking.Order.Details
        OrderId = fitVolumePacking.Order.OrderId
        Toys = newPackedToy::fitVolumePacking.Order.Toys      
      }
    let newPacking =
      {       
        SetOfExtremePoints = newExtremePoints
        Order = newOrder
      }
    packOrderInBoxRecursive (box, tail, newPacking, convexHull, maxIndexSoFar)
    | [] -> packing

let packOrderInBox (box:Box, toys:Toy list, packing:Packing) =
  let accumulator = 0M<centimeters^3>
  let initialConvexHull =
    {
        Dimensions = 
          {
            Length = 0M<centimeters>
            Width = 0M<centimeters>
            Height = 0M<centimeters>
          }
    }
  let initialPoint = 
   {
      X = 0M<centimeters>
      Y = 0M<centimeters>
      Z = 0M<centimeters>
   }
  let maxIndexSoFar = (0M<centimeters^2>, initialPoint, initialConvexHull)  
  let newPacking = packOrderInBoxRecursive(box, toys, packing, initialConvexHull, maxIndexSoFar)
  newPacking

let rec algorithmEvaluation (boxes:Box list, toys:Toy list, packing:Packing, convexHull:ConvexHull, maxIndexSoFar) =
  let accumulator = 0M<centimeters^3>
  match boxes with
  | head :: tail ->
      let currentBoxVolume = volume head.Dimensions.Length head.Dimensions.Width head.Dimensions.Height
      let currentOrderVolume = getToysVolume(toys, accumulator)
      if (currentBoxVolume >= currentOrderVolume) then
        packOrderInBox(head, toys, packing)
      else
        algorithmEvaluation(tail, toys, packing, convexHull, maxIndexSoFar)
  | [] -> packing

let sortMethodForBoxes (box1:Box) (box2:Box) =
  let box1Volume = volume box1.Dimensions.Length box1.Dimensions.Width box1.Dimensions.Height
  let box2Volume = volume box2.Dimensions.Length box2.Dimensions.Width box2.Dimensions.Height
  if (box1Volume < box2Volume) then
    1
  else
    -1

let firstFitDecreasingAlgorithm(boxes:Box list, toys:Toy list, packing:Packing) =
  let sortedBoxesByVolume = boxes |> List.sortWith (sortMethodForBoxes)
  sortedBoxesByVolume

let getPoint (point:Point) =
  match point with 
  |  point -> Success point

let getPacking (packing:Packing) =
  match packing with 
  |  packing -> Success packing

let getConvexHull (convexHull:ConvexHull) =
  match convexHull with 
  |  convexHull -> Success convexHull

let blankBox =
     {
        Id = 
          {
              BoxId = "0";
          }
        Details =
          {
            Name = "Box0"
            Description = "Blank box"
          }
        Dimensions =
          {
             Length = 0M<centimeters>;
             Width = 0M<centimeters>;
             Height = 0M<centimeters>
          };
    }

let blankBoxes =
    [
      blankBox
    ]

let getBox world =
  let tempValue = world.AvailableBoxes |> Seq.head
  match Some(tempValue) with 
  |  Some (KeyValue(k, v)) -> Success v
  |  None -> Failure "Box does not exist"

let getBoxZ2 world =
  match Some(world.AvailableBoxes |> Seq.map(fun (KeyValue(k,v)) -> v)) with 
  |  Some v -> v |> Seq.toList
  |  None -> blankBoxes

let getBoxes world =
  let tempValues = world.AvailableBoxes |> Seq.map(fun (KeyValue(k,v)) -> v)
  match Some(tempValues) with 
  |  Some v -> Success v
  |  None -> Failure "Box does not exist"  

let getBox2 world =
  match Some(world.AvailableBoxes |> Seq.head) with 
  |  Some (KeyValue(k, v)) -> v
  |  None -> blankBox  

let getToys world =
  match Some world.Order.Toys with 
  |  Some toys -> Success toys
  |  None -> Failure "Toys do not exist"

let executeFitnessEvaluation world =
  let packing =
    {       
      SetOfExtremePoints = initialSetOfExtremePoints
      Order = blankOrder
    }
  let box = getBox2 world
  let initialConvexHull =
    {
        Dimensions = 
          {
            Length = 0M<centimeters>
            Width = 0M<centimeters>
            Height = 0M<centimeters>
          }
    }
  let initialPoint = 
   {
      X = 0M<centimeters>
      Y = 0M<centimeters>
      Z = 0M<centimeters>
   }
  let maxIndexSoFar = (0M<centimeters>, initialPoint, initialConvexHull)
  fitnessEvaluation(world.Order.Toys, box, packing, initialConvexHull, maxIndexSoFar)


let executeAlgorithmEvaluation world =
  let packing =
    {       
      SetOfExtremePoints = initialSetOfExtremePoints
      Order = blankOrder
    }
  let box = getBox2 world
  let initialConvexHull =
    {
        Dimensions = 
          {
            Length = 0M<centimeters>
            Width = 0M<centimeters>
            Height = 0M<centimeters>
          }
    }
  let initialPoint = 
   {
      X = 0M<centimeters>
      Y = 0M<centimeters>
      Z = 0M<centimeters>
   }
  let maxIndexSoFar = (-1000M<centimeters>, initialPoint, initialConvexHull)
  let boxes = getBoxZ2 world
  algorithmEvaluation(boxes, world.Order.Toys, packing, initialConvexHull, maxIndexSoFar)

let getToy world =
  match Some(world.Order.Toys |> Seq.head) with 
  |  Some toy -> Success toy
  |  None -> Failure "Toy does not exist"

let describeDetails details =
  printfn "\n\n%s\n\n%s\n\n" details.Name details.Description
  sprintf "\n\n%s\n\n%s\n\n" details.Name details.Description

let extractDetailsFromToy (toy : Toy) =
  toy.Details

let extractDetailsFromBox (box : Box) =
  box.Details

let describeCurrentToy world =
  getToy world
  |> (bind (switch extractDetailsFromToy) >> bind (switch describeDetails))

let describeCurrentBox world =
  getBox world
  |> (bind (switch extractDetailsFromBox) >> bind (switch describeDetails))  

let customerOrder_WuExample2 =
  {
    Details =
      {
        Name = "Order#1Name";
        Description = "Order#1Description"
      }
    OrderId = 
      {
        OrderId = "Order#1";
      }
    Toys = 
      [
        {
          Details = 
            {
              Name = "Toy#1Name";
              Description = "Toy#1Description"
            }
          Dimensions = 
            {
              Length = 5M<centimeters>
              Width = 6M<centimeters>
              Height = 7M<centimeters>
            }
          Weight = 1M<kilograms>
          Origin =             
            { 
              X = 0M<centimeters>
              Y = 0M<centimeters>
              Z = 0M<centimeters>
            }
        }
        {
          Details = 
            {
              Name = "Toy#2Name";
              Description = "Toy#2Description"
            };                 
          Dimensions = 
            {
              Length = 4M<centimeters>
              Width = 3M<centimeters>
              Height = 5M<centimeters>
            }
          Weight = 2M<kilograms>
          Origin =             
            { 
              X = 0M<centimeters>
              Y = 0M<centimeters>
              Z = 0M<centimeters>
            }
        }
        {
          Details = 
            {
              Name = "Toy#3Name";
              Description = "Toy#3Description"
            };                 
          Dimensions = 
            {
              Length = 4M<centimeters>
              Width = 3M<centimeters>
              Height = 5M<centimeters>
            }
          Weight = 2M<kilograms>
          Origin =             
            { 
              X = 0M<centimeters>
              Y = 0M<centimeters>
              Z = 0M<centimeters>
            }
        }
        {
          Details = 
            {
              Name = "Toy#4Name";
              Description = "Toy#4Description"
            };                 
          Dimensions = 
            {
              Length = 1M<centimeters>
              Width = 3M<centimeters>
              Height = 2M<centimeters>
            }
          Weight = 2M<kilograms>
          Origin =             
            { 
              X = 0M<centimeters>
              Y = 0M<centimeters>
              Z = 0M<centimeters>
            }
        }                
      ]
    }            

let allBoxes_WuExample3 = 
              [
                {
                    Id = 
                      {
                          BoxId = "7_14_10";
                      }
                    Details =
                      {
                        Name = "Box7"
                        Description = "7 X 14 X 10"
                      }
                    Dimensions =
                      {
                         Length = 7M<centimeters>;  // Blue Z
                         Width = 14M<centimeters>;  // Red X
                         Height = 10M<centimeters>  // Green Y
                      };
                }   
              ]

let algorithmWorld =
  {
    AvailableBoxes =
      allBoxes_WuExample3
      |> Seq.map (fun box -> (box.Id, box))
      |> Map.ofSeq
    Customer = customer
    Order = customerOrder_WuExample2       
  }
  
(**
Initial settings and helper functions
------------------------------------

Note: this sample is intended to be a direct port of the original
and doesn't attempt to refactor the original to be more "functional".
*)

let worldWidth = 256
let worldDepth = 256
let worldHalfWidth = worldWidth / 2
let worldHalfDepth = worldDepth / 2

let clock = Three.Clock()

// We can also use `System.Random`, but the native JS `Math.random`
// will be a bit more performant here.
let inline rand() = JS.Math.random()

(**
Using the perlin library (ImprovedNoise script) define the peaks
of the mountains in our random terrain.
*)

let generateHeight width height =
    let size = width * height
    let data:float[] = Array.zeroCreate size
    let perlin = ImprovedNoise()
    let mutable quality = 1.0
    let z = rand() * 100.0

    for j in 0..3 do
        for i in 0..(size-1) do
            let x = i % width
            let y = i / width
            let noise =
                perlin.noise(float x / quality, float y / quality, z)
                    * quality * 1.75
            data.[i] <- data.[i] + (JS.Math.abs ( noise ))
        quality <- quality * 5.0
    data

(**
To generate the textures for the terrain, we'll be using a canvas element
to draw the image and later pass it directly to THREE.Texture class.
*)

let generateTexture (data:float[]) (width:int) (height:int) =
    let vector3 = Three.Vector3(0.0, 0.0, 0.0)
    let sun = (Three.Vector3(1.0, 1.0, 1.0) :> Three.Vector).normalize()

    let canvas = Browser.document.createElement_canvas()
    canvas.width <- float width
    canvas.height <- float height

    let context = canvas.getContext_2d()
    context.fillStyle <- U3.Case1 "#000"
    context.fillRect(0.0, 0.0, float width, float height)

    let image = context.getImageData(
                    0.0, 0.0, canvas.width, canvas.height)
    let imageData = image.data

    let mutable i = 0
    let mutable j = 0
    let mutable l = int imageData.length

    while i < l do
        // Note: data elements -2 and -1 are accessed here at the start
        // of the loop. It's a bug in the original (producing NaN after
        // normalize()), but JS just keeps on truckin'. There is a similar
        // issue with z.  The result is that imageData is set to zero (black)
        // in these cases
        vector3.x <- data.[ j - 2 ] - data.[ j + 2 ]
        vector3.y <- 2.0
        vector3.z <- data.[ j - width * 2 ] - data.[ j + width * 2 ]
        (vector3 :> Three.Vector).normalize() |> ignore

        let shade = vector3.dot(sun :?> Three.Vector3)

        imageData.[ i ] <-
            (96.0 + shade * 128.0) * (0.5 + data.[ j ] * 0.007)
        imageData.[ i + 1 ] <-
            (32.0 + shade * 96.0) * (0.5 + data.[ j ] * 0.007)
        imageData.[ i + 2 ] <-
            (shade * 96.0) * (0.5 + data.[ j ] * 0.007)

        i <- i + 4
        j <- j + 1

    context.putImageData( image, 0.0, 0.0 );

    let canvasScaled = Browser.document.createElement_canvas()
    canvasScaled.width <- float(width * 4)
    canvasScaled.height <- float(height * 4)

    let context = canvasScaled.getContext_2d()
    context.scale(4.0,4.0)
    context.drawImage(U3.Case2 canvas, 0.0, 0.0)

    let image = context.getImageData(
                    0.0, 0.0, canvasScaled.width, canvasScaled.height)
    let imageData = image.data

    let mutable i = 0
    let mutable l = int imageData.length
    while i < l do
        // I presume double-not is used here for this reason:
        // http://james.padolsey.com/javascript/double-bitwise-not/
        let v = ~~~ (~~~ (rand() * 5.0 |> int)) |> float
        imageData.[ i ] <- imageData.[ i ] + v
        imageData.[ i + 1 ] <- imageData.[ i + 1 ] + v
        imageData.[ i + 2 ] <- imageData.[ i + 2 ] + v
        i <- i + 4

    context.putImageData(image, 0.0, 0.0)
    canvasScaled

let scale = 2.0

let convertBoxesToWebGL(box:Box, scene:Three.Scene) = 
  let color = new Three.Color(255.0, 0.0, 0.0)  // Red
  let meshMaterial = new Three.MeshBasicMaterial()
  meshMaterial.color <- color
  meshMaterial.wireframe <- true
  meshMaterial.transparent <- true
  meshMaterial.opacity <- 0.08   
  let boxGeometry = new Three.BoxGeometry(
                     float (box.Dimensions.Width/1M<centimeters>),
                     float (box.Dimensions.Height/1M<centimeters>),
                     float (box.Dimensions.Length/1M<centimeters>))
  let bufferGeometry = new Three.BufferGeometry()
  let convert = bufferGeometry.fromGeometry(boxGeometry);
  let box = new Three.Mesh(convert, meshMaterial)
  box.scale.set(scale, scale, scale) |> ignore
  //box.position.x <- 0.0
  //box.position.y <- 0.0
  //box.position.z <- 0.0 
  scene.add(box)

let calculateWebGLPositionFromWuCoordinates (description:string, boxDimension:decimal<centimeters>, toyPosition:decimal<centimeters>, toyDimension:decimal<centimeters>, scale: decimal) =
  let graphicCenter = 0.0M<centimeters>
  let halfTheBox = 0.5M * boxDimension * scale
  let halfTheToy = 0.5M * toyDimension * scale
  let graphicPosition = graphicCenter - halfTheBox + (toyPosition  * scale) + halfTheToy
  printfn "%O- boxDim %O .5boxDim %O toyPos %O / halfToy %O graphPos %O" 
    description boxDimension halfTheBox toyPosition halfTheToy graphicPosition
  Math.Round(graphicPosition/1M<centimeters>, 3)

let printColor description color =
  //let color = Three.Color(10.0, 245.0, 250.0) // Green
  printfn "toy: %O" description
  printfn "Color: %O" color

let rec convertToysToWebGL(toys:Toy list, scene:Three.Scene, random:Random, box:Box, meshList:Three.Mesh list) = 
  match toys with
  | head :: tail ->
      if head.Details.Description.Contains("#") then
        //let red, green, blue = random.Next(256), random.Next(256), random.Next(256)
        //let color = new Three.Color((float red), (float green), (float blue))  // Random color
        //let color = new Three.Color((float red), (float green), 0.0)  // Red
        let mutable color = new Three.Color(255.0, 255.0, 255.0)
        if head.Details.Description.Contains("#1") then
          printColor head.Details.Description "Red"
          color <- new Three.Color(255.0, 0.0, 0.0) // Red
        else if head.Details.Description.Contains("#2") then
          printColor head.Details.Description "Green"
          color <- new Three.Color(0.0, 255.0, 0.0) // Green
        else if head.Details.Description.Contains("#3") then
          printColor head.Details.Description "Blue"
          color <- new Three.Color(0.0, 0.0, 255.0) // Blue
        else
          printColor head.Details.Description "White"
  
         // Red
        //printfn "%s" "red: " + red
        //console.log()
  
        let meshMaterial = new Three.MeshBasicMaterial()
        meshMaterial.color <- color
        meshMaterial.transparent <- true
        meshMaterial.opacity <- 0.4         
        let boxGeometry = new Three.BoxGeometry(
                           float (head.Dimensions.Width/1M<centimeters>),
                           float (head.Dimensions.Height/1M<centimeters>),
                           float (head.Dimensions.Length/1M<centimeters>))
        let bufferGeometry = new Three.BufferGeometry()
        let convert = bufferGeometry.fromGeometry(boxGeometry);
        let toy = new Three.Mesh(convert, meshMaterial)
        printfn "ox %O oy %O oz %O" head.Origin.X head.Origin.Y head.Origin.Z
        printfn "dx %O dy %O dz %O" head.Dimensions.Width head.Dimensions.Height head.Dimensions.Length 
        toy.scale.set(scale, scale, scale) |> ignore
        toy.position.x <- float(calculateWebGLPositionFromWuCoordinates("x/width", box.Dimensions.Width, head.Origin.Y, head.Dimensions.Width, (decimal)scale))
        toy.position.y <- float(calculateWebGLPositionFromWuCoordinates("y/height", box.Dimensions.Height, head.Origin.Z, head.Dimensions.Height, (decimal)scale))
        toy.position.z <- float(calculateWebGLPositionFromWuCoordinates("z/length", box.Dimensions.Length, head.Origin.X, head.Dimensions.Length, (decimal)scale))
        printfn "x %O y %O z %O" toy.position.x toy.position.y toy.position.z
  
        scene.add(toy)

        convertToysToWebGL(tail, scene, random, box, toy::meshList)
      else
        convertToysToWebGL(tail, scene, random, box, meshList)
  | [] -> meshList

let convertAlgorithmToWebGl(algorithm:World, scene:Three.Scene) =
  let boxes = getBoxZ2 algorithm
  let oneBox = (boxes |> Seq.head)
  convertBoxesToWebGL(oneBox, scene)
  let random = new Random()
  //let toy = (algorithm.Order.Toys |> Seq.head)
  //let toys = [algorithm.Order.Toys.[2]]
  let helper = new Three.AxisHelper(30.0)
  scene.add(helper)
  let meshList = []
  let meshListReturn = convertToysToWebGL(algorithm.Order.Toys, scene, random, oneBox, meshList)
  let mutable count = 1
  for mesh in meshListReturn do
    //printfn "add a mesh %O" count
    count <- count + 1
    //scene.add(mesh)
  let myColor = U2.Case2 "#3C3C3C"
  let ambientLight = new Three.AmbientLight(myColor, 2.0)
  scene.add(ambientLight)
  let directionalLight = new Three.DirectionalLight(U2.Case2 "#3C3C3C", 0.5)
  scene.add(directionalLight) |> ignore

(**
Initialize elements
-------------------

Here we initialize the elements necessary to draw the scene:
the renderer, the scene itself, a camera and controls to move it.

Note the use of a compiler directive: normally we take the whole window space,
but if we are in the tutorial we should leave space for the explanations.
*)

let init() =
    #if TUTORIAL
    let getWidth() = 800.
    let getHeight() = 450.
    #else
    let getWidth() = Browser.window.innerWidth
    let getHeight() = Browser.window.innerHeight
    #endif

    let container = Browser.document.getElementById("container")
    let camera = Three.PerspectiveCamera(
                    60.0, getWidth() / getHeight(), 1.0, 20000.0)
    let scene = Three.Scene()

    let renderer = Three.WebGLRenderer()
    renderer.setClearColor("#bfd1e5")
    (renderer :> Three.Renderer).setSize(getWidth(), getHeight())
    let domElement = (renderer :> Three.Renderer).domElement
    container.innerHTML <- ""
    container.appendChild(domElement) |> ignore

    let controls = OrbitControls.Create(camera :> Three.Camera, domElement :> Browser.HTMLElement)
    //controls.movementSpeed <- 1000.0
    //controls.lookSpeed <- 0.1

    let data = generateHeight worldWidth worldDepth

    camera.position.y <- 50.

    let geometry = Three.PlaneBufferGeometry(
                        7500.0, 7500.0,
                        float (worldWidth - 1), float (worldDepth - 1))
    geometry.applyMatrix(Three.Matrix4()
            .makeRotationX(-JS.Math.PI / 2.0))
            |> ignore

    let vertices = geometry.getAttribute("position")
                    |> unbox<Three.BufferAttribute>
    let vertices = vertices.array
    let l = int vertices.length
    let mutable i = 0
    let mutable j = 0
    while i < l do
        vertices.[j + 1] <- data.[i] * 10.0
        i <- i + 1
        j <- j + 3

    let texCanvas = generateTexture data worldWidth worldDepth
    let texture = Three.Texture(
                    U3.Case2 texCanvas, Three.UVMapping,
                    Three.ClampToEdgeWrapping,
                    Three.ClampToEdgeWrapping)
    texture.needsUpdate <- true

    // We use createEmpty here to create an empty object used to set
    // configuration parameters. The type qualifier indicates what fields
    // we will be able to set on the resulting object. For those fields that
    // are enum types, the possible values are usually found in three globals.
    let matProps = createEmpty<Three.MeshBasicMaterialParameters>
    matProps.map <- Some texture

    let mesh = Three.Mesh(geometry, Three.MeshBasicMaterial(matProps))
    //scene.add mesh

    let packing = executeAlgorithmEvaluation algorithmWorld
    let algorithmWorldPacked =
      {
        AvailableBoxes =
          allBoxes_WuExample3
          |> Seq.map (fun box -> (box.Id, box))
          |> Map.ofSeq
        Customer = customer
        Order = packing.Order       
      }
    convertAlgorithmToWebGl(algorithmWorldPacked, scene) |> ignore

    let onWindowResize(e:Browser.UIEvent):obj =
        camera.aspect <- getWidth() / getHeight()
        camera.updateProjectionMatrix()
        (renderer :> Three.Renderer).setSize(getWidth(), getHeight())
        controls.handleResize()
        null

    printfn "here baby"

    Browser.window.addEventListener_resize(
        Func<_,_> onWindowResize, false)

    renderer, scene, camera, controls

let renderer,scene,camera,controls = init()


(**
Start animation
---------------

Now the only thing left is to start the animation. Note we use the
`window.requestAnimationFrame` function here, this will make sure
`animate` is executed at a proper frame rate.
*)

let render() =
    //controls.update(clock.getDelta())
    controls.update()
    renderer.render(scene, camera)

let rec animate (dt:float) =
    Browser.window.requestAnimationFrame(Func<_,_> animate)
    |> ignore
    render()

//let exporter = new Three.Obj.OBJExporter();
//exporter.parse(mesh);

// kick it off
animate(0.0)
