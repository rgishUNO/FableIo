(**
 - title: WebGL Bin Packing
 - tagline: A 3D algorithm right in the browser
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
In addition this demo uses custom scripts for ImprovedNoise, FirstPersonControls, and OrbitControls.
We'll write the mappings for those three inline.
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

type IOrbitControls =
    abstract handleResize: unit -> unit
    abstract update: unit -> unit

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

 type CageBin =
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

 type ReferencePoints = 
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

type ToyWithReferencePoints =
  {
      Toy: Toy * ReferencePoints
  }

type ToyWithReferencePointsAndOrientations =
  {
      ToyWithReferencePoints: ToyWithReferencePoints * Orientation
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
      SetOfReferencePoints: Point list
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

let initialSetOfReferencePoints = 
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
  | Success s -> printf "%s" s
  | Failure f -> printf "%s" f

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

let getMaxReferencePointsHeight (referencePoints:ReferencePoints) =
  let leftFrontRemoveMeasure = referencePoints.LeftFrontBottomPoint.Z / 1M<centimeters>
  let rightBehindRemoveMeasure = referencePoints.RightBehindBottomPoint.Z / 1M<centimeters>
  let leftBehindRemoveMeasure = referencePoints.LeftBehindTopPoint.Z / 1M<centimeters>
  let list = [leftFrontRemoveMeasure; rightBehindRemoveMeasure; leftBehindRemoveMeasure]
  let maxValueOfList = list |> List.max
  let maxWithMeasure = maxValueOfList * 1M<centimeters>
  maxWithMeasure

let getPackedToyCageBinHeight packedToy =
  let packedToyCageBinHeight = packedToy.Origin.Y + packedToy.Dimensions.Height
  packedToyCageBinHeight

let getPackedToyCageBinWidth packedToy =
  let packedToyCageBinWidth = packedToy.Origin.X + packedToy.Dimensions.Width
  packedToyCageBinWidth

let calculateCageBin packedToy maxHeightSoFar = 
      let toyHeight = getPackedToyCageBinHeight(packedToy)
      let list = [toyHeight; maxHeightSoFar]
      let maxValueOfList = list |> List.max
      maxValueOfList

let rec getHeightOfCageBinOfCurrentlyPackedToys packedToys  maxHeightSoFar =
  match packedToys with
  | head :: tail ->
      let maxHeightWithMeasure = calculateCageBin head maxHeightSoFar
      getHeightOfCageBinOfCurrentlyPackedToys tail maxHeightWithMeasure
  | [] -> maxHeightSoFar

let getHeightOfCageBinOfCurrentPacking (packedToys:Packing, maxHeightSoFar:decimal<centimeters>) =
  match packedToys.Order.Toys with
  | head :: tail ->
      let maxHeightWithMeasure = calculateCageBin head maxHeightSoFar
      getHeightOfCageBinOfCurrentlyPackedToys tail maxHeightWithMeasure
  | [] -> maxHeightSoFar

let volume length width height = 
  length * width * height

let rec getToysVolume (toys:Toy list, accumulator) =
  match toys with
  | head :: tail ->
      let toyVolumePacking = volume head.Dimensions.Length head.Dimensions.Width head.Dimensions.Height
      getToysVolume(tail, (accumulator + toyVolumePacking))
  | [] -> accumulator

let calculateCageBinWithCurrentToy(referencePoint:Point, toy:Toy, cageBin:CageBin) =
  let toyLength = referencePoint.X + toy.Dimensions.Length
  let listLength = [toyLength; cageBin.Dimensions.Length]
  let maxValueOfListLength = listLength |> List.max

  let toyWidth = referencePoint.Y + toy.Dimensions.Width
  let listWidth = [toyWidth; cageBin.Dimensions.Width]
  let maxValueOfListWidth = listWidth |> List.max

  let toyHeight = referencePoint.Z + toy.Dimensions.Height
  let listHeight = [toyHeight; cageBin.Dimensions.Height]
  let maxValueOfListHeight = listHeight |> List.max

  let newCageBin =
    {
      Dimensions = 
        {
          Length = maxValueOfListLength
          Width = maxValueOfListWidth
          Height = maxValueOfListHeight
        }
    }
  newCageBin

let cageBinLength(referencePoint:Point, toy:Toy, currentCageBinLengthMax) =
  let toyLength = referencePoint.X + toy.Dimensions.Length
  let list = [toyLength; currentCageBinLengthMax]
  let maxValueOfList = list |> List.max
  maxValueOfList

let cageBinWidth(referencePoint:Point, toy:Toy, currentCageBinWidthMax) =
  let toyWidth = referencePoint.Y + toy.Dimensions.Width
  let list = [toyWidth; currentCageBinWidthMax]
  let maxValueOfList = list |> List.max
  maxValueOfList

let cageBinHeight(referencePoint:Point, toy:Toy, currentCageBinHeightMax) =
  let toyHeight = referencePoint.Z + toy.Dimensions.Height
  let list = [toyHeight; currentCageBinHeightMax]
  let maxValueOfList = list |> List.max
  maxValueOfList

let tolerance = 0.001M<centimeters>

let roomInBoxForNewToyVersusBox (dimensions:Dimensions, toy:Toy) =
  let lengthAvailability = dimensions.Length - (toy.Origin.X + toy.Dimensions.Length) >= tolerance
  let widthAvailability = dimensions.Width - (toy.Origin.Y + toy.Dimensions.Width)  >= tolerance
  let heightAvailability = dimensions.Height - (toy.Origin.Z + toy.Dimensions.Height) >= tolerance
  let temp = heightAvailability
  lengthAvailability && widthAvailability && heightAvailability

let packedToyRightGreaterThanNewToyLeft (packedToyOrigin:decimal<centimeters>, packedToyDimension:decimal<centimeters>, toyOrigin:decimal<centimeters>) =
  let greater = toyOrigin < packedToyOrigin + packedToyDimension
  greater

let packedToyLeftLessThanNewToyRight (packedToyOrigin:decimal<centimeters>, toyOrigin:decimal<centimeters>, toyDimension:decimal<centimeters>) =
  let less = packedToyOrigin < toyOrigin + toyDimension
  less

let crossOnTheXPlane (packedToy:Toy, toy:Toy) =
  let cross =
    packedToyRightGreaterThanNewToyLeft(packedToy.Origin.X, packedToy.Dimensions.Length, toy.Origin.X)
    && packedToyLeftLessThanNewToyRight(packedToy.Origin.X, toy.Origin.X, toy.Dimensions.Length)
  cross

let crossOnTheYPlane (packedToy:Toy, toy:Toy) =
  let cross =
    packedToyRightGreaterThanNewToyLeft(packedToy.Origin.Y, packedToy.Dimensions.Width, toy.Origin.Y)
    && packedToyLeftLessThanNewToyRight(packedToy.Origin.Y, toy.Origin.Y, toy.Dimensions.Width)
  cross

let crossOnTheZPlane (packedToy:Toy, toy:Toy) =
  let cross =
    packedToyRightGreaterThanNewToyLeft(packedToy.Origin.Z, packedToy.Dimensions.Height, toy.Origin.Z)
    && packedToyLeftLessThanNewToyRight(packedToy.Origin.Z, toy.Origin.Z, toy.Dimensions.Height)
  cross

let calculateAvailability (packedToy:Toy, toy:Toy) =
  let availability = not (crossOnTheXPlane(packedToy, toy)
                          && crossOnTheYPlane(packedToy, toy)
                          && crossOnTheZPlane(packedToy, toy))
  availability

let rec roomInBoxForNewToyVersusAlreadyPackedToysRecursive (dimensions:Dimensions, toy:Toy, packedToys:Toy list, accumulator:bool list) =
  match packedToys with
  | head :: tail ->
      let availability = calculateAvailability(head, toy)
      roomInBoxForNewToyVersusAlreadyPackedToysRecursive(dimensions, toy, tail, availability::accumulator)
  | [] -> accumulator
                                    
let roomInBoxForNewToyVersusAlreadyPackedToys (dimensions:Dimensions, toy:Toy, packing:Packing) =
  let accumulator = []
  match packing.Order.Toys with
  | head :: tail ->
      let availability = calculateAvailability(head, toy)
      let temp = availability
      roomInBoxForNewToyVersusAlreadyPackedToysRecursive(dimensions, toy, tail, availability::accumulator)
  | [] -> accumulator

let roomInBoxForNewToy (dimensions:Dimensions, toy:Toy, packing:Packing) =
  let versusBox = roomInBoxForNewToyVersusBox(dimensions, toy)

  // If we fail already fitting in the box, don't worry about sifting through packed toys, just default to a false array
  let versusAlreadyPackedToys = if not versusBox then
                                  false::[]
                                else
                                  roomInBoxForNewToyVersusAlreadyPackedToys(dimensions, toy, packing)

  let versusPacking = versusAlreadyPackedToys 
                      |> List.where(fun x -> x = false) 
                      |> List.isEmpty
  (versusBox && versusPacking)

let attemptingToDivideByZero volumeOfToysPackedSoFarIncludingCurrentToy =
  volumeOfToysPackedSoFarIncludingCurrentToy = 0M<centimeters^3>

/// Measure of height over the fill rate of the current convex hull
let packingIndex(length:decimal<centimeters>, width:decimal<centimeters>, height:decimal<centimeters>, volumeOfToysPackedSoFarIncludingCurrentToy:decimal<centimeters^3>) =
  let heightSquared = decimal (height/1M<centimeters>) * decimal (height/1M<centimeters>)
  let heightSquaredMeasure = heightSquared * 1M<centimeters>
  let numerator = length + width + heightSquaredMeasure
  let index =
    if (attemptingToDivideByZero volumeOfToysPackedSoFarIncludingCurrentToy) then
      numerator/(1M * 1M<centimeters^3>)
    else
      (numerator/volumeOfToysPackedSoFarIncludingCurrentToy)
  index

let packingIndexZ(length:decimal<centimeters>, width:decimal<centimeters>, height:decimal<centimeters>, volumeOfToysPackedSoFarIncludingCurrentToy:decimal<centimeters^3>, lowestZRatio:decimal<centimeters>, roomRatio:decimal<centimeters>, maxSoFar) =
  let heightDifferential = height * lowestZRatio
  let numerator = length * width * heightDifferential * roomRatio

  let index =
    if (attemptingToDivideByZero volumeOfToysPackedSoFarIncludingCurrentToy) then
      numerator/(1M * 1M<centimeters^3>)
    else
      (numerator/volumeOfToysPackedSoFarIncludingCurrentToy)

  let (indexMax, _, _) = maxSoFar
  let roomRatioBasedIndex = if roomRatio = 0M<centimeters> then
                              indexMax - 1000M<centimeters^2>
                            else
                              index
  roomRatioBasedIndex

let maxPackingIndex x y = 
  let (a, b, c) = x
  let (d, e, f) = y
  if a < d then (d, e, f) else (a, b, c)

let minPackingIndex x y = 
  let (a, b, c) = x
  let (d, e, f) = y
  if a > d then (d, e, f) else (a, b, c)

let rec calculateIndex (currentToy:Toy, referencePoint:Point, volume:decimal<centimeters^3>, cageBin:CageBin, maxSoFar) =
  let cageBinPoint = 
    calculateCageBinWithCurrentToy(referencePoint, currentToy, cageBin)
  let lpj = cageBinPoint.Dimensions.Length
  let wpj = cageBinPoint.Dimensions.Width
  let hpj = cageBinPoint.Dimensions.Height
  let index = (packingIndex(lpj, wpj, hpj, volume), referencePoint, cageBinPoint)

  let min = minPackingIndex index maxSoFar
  min

let rec calculateIndexZValue (currentToy:Toy, box:Box, referencePoint:Point, volume:decimal<centimeters^3>, cageBin:CageBin, maxSoFar, lowestZRatio, packing:Packing) =
  let cageBinPoint = 
    calculateCageBinWithCurrentToy(referencePoint, currentToy, cageBin)
  let lpj = cageBinPoint.Dimensions.Length
  let wpj = cageBinPoint.Dimensions.Width
  let hpj = cageBinPoint.Dimensions.Height
  let newLowestZDifferential = 1M<centimeters> - max (referencePoint.Z - lowestZRatio) 0.1M<centimeters>
  let currentToyAtReferencePoint = 
        {
          Details = currentToy.Details
          Dimensions = currentToy.Dimensions
          Weight = currentToy.Weight
          Origin =             
            { 
              X = referencePoint.X
              Y = referencePoint.Y
              Z = referencePoint.Z
            }
        }
  
  let roomAvailable =  roomInBoxForNewToy (box.Dimensions, currentToyAtReferencePoint, packing)
  let roomRatio =
    if roomAvailable then
      1M<centimeters>
    else
      0M<centimeters>    
  let index = (packingIndexZ(lpj, wpj, hpj, volume, newLowestZDifferential, roomRatio, maxSoFar), referencePoint, cageBinPoint)

  let max = maxPackingIndex index maxSoFar
  max

let rec calculateIndexViaListOfReferencePointsRecursive (currentToy:Toy, referencePoints:Point list, volume:decimal<centimeters^3>, maxIndexSoFar, cageBin:CageBin) =
  match referencePoints with
  | head :: tail ->
      let referencePointsIndex = calculateIndex(currentToy, head, volume, cageBin, maxIndexSoFar) 
      calculateIndexViaListOfReferencePointsRecursive(currentToy, tail, volume, referencePointsIndex, cageBin)
  | [] -> maxIndexSoFar

let rec calculateIndexViaListOfReferencePointsZValuesRecursive (box:Box, currentToy:Toy, referencePoints:Point list, volume:decimal<centimeters^3>, maxIndexSoFar, cageBin:CageBin, lowestZValue:decimal<centimeters>, packing:Packing) =
  match referencePoints with
  | head :: tail ->
      let referencePointsIndex = calculateIndexZValue(currentToy, box, head, volume, cageBin, maxIndexSoFar, lowestZValue, packing) 
      calculateIndexViaListOfReferencePointsZValuesRecursive(box, currentToy, tail, volume, referencePointsIndex, cageBin, lowestZValue, packing)
  | [] -> maxIndexSoFar

let sortMethodForReferencePointZValues (point1:Point) (point2:Point)=
  if (point1.Z > point2.Z) then
    1
  else if (point1.Z < point2.Z) then
    -1
  else
    if (point1.Z = point2.Z) then
      0
    else
      -1

let  calculateIndexViaListOfReferencePointsZValues (currentToy:Toy, box:Box, setOfReferencePoints:Point list, volume:decimal<centimeters^3>, maxIndexSoFar, cageBin:CageBin, packing:Packing) =
  let sortedSetOfReferencePoints = (setOfReferencePoints |> List.sortWith(sortMethodForReferencePointZValues))
  let lowestZValue = sortedSetOfReferencePoints.[0].Z
  match sortedSetOfReferencePoints with
  | head :: tail ->
      let referencePointsIndex = calculateIndexZValue(currentToy, box, head, volume, cageBin, maxIndexSoFar, lowestZValue, packing)
      calculateIndexViaListOfReferencePointsZValuesRecursive(box, currentToy, tail, volume, referencePointsIndex, cageBin, lowestZValue, packing)
  | [] -> maxIndexSoFar

let  calculateIndexViaListOfReferencePoints (currentToy:Toy, setOfReferencePoints:Point list, volume:decimal<centimeters^3>, maxIndexSoFar, cageBin:CageBin, explicitPoint:Point) =
  if explicitPoint.Z <> 0M<centimeters> then
    let (index, indexPoint, cageBin) = maxIndexSoFar
    let exlicitIndex = (index, explicitPoint, cageBin)
    exlicitIndex
  else
    match setOfReferencePoints with
    | head :: tail ->
        let referencePointsIndex = calculateIndex(currentToy, head, volume, cageBin, maxIndexSoFar)
        calculateIndexViaListOfReferencePointsRecursive(currentToy, tail, volume, referencePointsIndex, cageBin)
    | [] -> maxIndexSoFar

let calculateToyReferencePoints(currentToy: Toy) =
  let setOfReferencePoints = 
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
  setOfReferencePoints

let revealDimensions dimensions =
  dimensions

let extractDimensionsFromBox (box : Box) =
  box.Dimensions

let noVolumeLeft remainingVolume =
  remainingVolume <= 0M<centimeters^3>

let blankPacking =
  {       
    SetOfReferencePoints = initialSetOfReferencePoints
    Order = blankOrder
  }

let doesBoxHaveEnoughFreeVolumeForToy (dimensions:Dimensions, toy:Toy, packing:Packing) =
  let toyVolume = volume toy.Dimensions.Length toy.Dimensions.Width toy.Dimensions.Height
  let boxVolume = volume dimensions.Length dimensions.Width dimensions.Height  
  let volumeLeft = boxVolume - toyVolume
  match volumeLeft with 
  |  i when noVolumeLeft i -> blankPacking
  |  _ -> packing

let rec fitnessEvaluation (toys:Toy list, box:Box, packing:Packing, cageBin:CageBin, maxIndexSoFar, explicitPoint:Point) =
  let accumulator = 0M<centimeters^3>
  match toys with
  | head :: tail ->
      let currentHeightOfCageBin = getHeightOfCageBinOfCurrentPacking(packing, cageBin.Dimensions.Height)
      let packedToyVolume = getToysVolume(packing.Order.Toys, accumulator)
      let currentToyVolume = volume head.Dimensions.Length head.Dimensions.Width head.Dimensions.Height
      let inclusiveToyVolume = packedToyVolume + currentToyVolume
      let index = calculateIndexViaListOfReferencePoints(head, packing.SetOfReferencePoints, inclusiveToyVolume, maxIndexSoFar, cageBin, explicitPoint)
      let fitVolumePacking = doesBoxHaveEnoughFreeVolumeForToy(box.Dimensions, head, packing)
      let (_, indexPoint, _) = index
      let newCageBin = calculateCageBinWithCurrentToy(indexPoint, head, cageBin)
      let setOfReferencePointsFound = 
        [
          {
            X = indexPoint.X
            Y = indexPoint.Y
            Z = indexPoint.Z
          }
        ]
      let filteredReferencePoints = (Set fitVolumePacking.SetOfReferencePoints) - (Set setOfReferencePointsFound) |> Set.toList
      let newPackedToy =
        {
          Details = head.Details
          Dimensions = head.Dimensions
          Weight = head.Weight
          Origin = indexPoint
        }
      let availability = roomInBoxForNewToy(box.Dimensions, newPackedToy, packing)
      let newReferencePointsFromNewlyPackedToy = calculateToyReferencePoints(newPackedToy)
      let newReferencePoints = (Set filteredReferencePoints) + (Set newReferencePointsFromNewlyPackedToy) |> Set.toList
      let newOrder =
        {
          Details = fitVolumePacking.Order.Details
          OrderId = fitVolumePacking.Order.OrderId
          Toys = newPackedToy::fitVolumePacking.Order.Toys      
        }
      let newPacking =
        {       
          SetOfReferencePoints = newReferencePoints
          Order = newOrder
        }
      fitnessEvaluation(tail, box, newPacking, newCageBin, index, explicitPoint)
  | [] -> packing

let utilizeCageBinPreviousPoint(filteredReferencePoints: Point list, cageBinPoint:Point list) = 
  let newReferencePoints = (Set filteredReferencePoints) + (Set cageBinPoint) |> Set.toList
  newReferencePoints

let cageBinPointAlreadyExists(cageBinReferencePoints:Point list, currentReferencePointsCount:Point list) =
  cageBinReferencePoints.Length = currentReferencePointsCount.Length

let rec packOrderInBoxWuScopedRecursive (box:Box, toys:Toy list, packing:Packing, cageBin:CageBin, currentIndexSoFar, discardZ, explicitPoint:Point) =
  let accumulator = 0M<centimeters^3>
  match toys with
  | head :: tail ->
    let currentToyVolume = volume head.Dimensions.Length head.Dimensions.Width head.Dimensions.Height
    let packedToyVolume = getToysVolume(packing.Order.Toys, accumulator)
    let inclusiveToyVolume = packedToyVolume + currentToyVolume
    let index = calculateIndexViaListOfReferencePoints(head, packing.SetOfReferencePoints, inclusiveToyVolume, currentIndexSoFar, cageBin, explicitPoint)
    let fitVolumePacking = doesBoxHaveEnoughFreeVolumeForToy(box.Dimensions, head, packing)
    let (_, indexPoint, _) = index
    let newCageBin = calculateCageBinWithCurrentToy(indexPoint, head, cageBin)
    let setOfReferencePointsFound = 
      [
        {
          X = indexPoint.X
          Y = indexPoint.Y
          Z = indexPoint.Z
        }
      ]
    let filteredReferencePoints = (Set fitVolumePacking.SetOfReferencePoints) - (Set setOfReferencePointsFound) |> Set.toList
    let newPackedToy =
      {
        Details = head.Details
        Dimensions = head.Dimensions
        Weight = head.Weight
        Origin = indexPoint
      }
    let availability = roomInBoxForNewToy(box.Dimensions, newPackedToy, packing)
    if availability then
        let newReferencePointsFromNewlyPackedToy = calculateToyReferencePoints(newPackedToy)
        let newReferencePoints = (Set filteredReferencePoints) + (Set newReferencePointsFromNewlyPackedToy) |> Set.toList
        let newOrder =
          {
            Details = fitVolumePacking.Order.Details
            OrderId = fitVolumePacking.Order.OrderId
            Toys = newPackedToy::fitVolumePacking.Order.Toys      
          }
        let newPacking =
          {       
            SetOfReferencePoints = newReferencePoints
            Order = newOrder
          }
        let (indexValue, indexPoint, _) = index
        let newMaxIndexSoFar = (indexValue, indexPoint, newCageBin)
        let resetExplicitPoint = 
          { 
            X = 0M<centimeters>
            Y = 0M<centimeters>
            Z = 0M<centimeters>
          }
        packOrderInBoxWuScopedRecursive (box, tail, newPacking, newCageBin, newMaxIndexSoFar, discardZ, resetExplicitPoint)
    else
        if explicitPoint.Z = 0M<centimeters> then
          let cageBinPoint =
              { 
                X = 0M<centimeters>
                Y = 0M<centimeters>
                Z =  cageBin.Dimensions.Height
              }
          packOrderInBoxWuScopedRecursive (box, head::tail, packing, cageBin, currentIndexSoFar, discardZ, cageBinPoint)
        else
          //if count hasn't increased, we are just looping, so bail to the stack  
          let newReferencePoints = (Set filteredReferencePoints) |> Set.toList
          let setNewPackedToyOutsideOfBox = 
            {
                Details = newPackedToy.Details                
                Dimensions = newPackedToy.Dimensions
                Weight = newPackedToy.Weight
                Origin =             
                { 
                    X = 0M<centimeters> - box.Dimensions.Length - 2M<centimeters>
                    Y = box.Dimensions.Width + 1M<centimeters>
                    Z = discardZ
                }
            } 
          let newOrder =
            {
              Details = fitVolumePacking.Order.Details
              OrderId = fitVolumePacking.Order.OrderId
              Toys = setNewPackedToyOutsideOfBox::fitVolumePacking.Order.Toys  
            }
          let newPacking =
            {       
              SetOfReferencePoints = newReferencePoints
              Order = newOrder
            }
          let newDiscardZ = discardZ + newPackedToy.Dimensions.Height
          packOrderInBoxWuScopedRecursive (box, tail, newPacking, cageBin, currentIndexSoFar, newDiscardZ, explicitPoint)
    | [] -> packing

let rec packOrderInBoxWuDerivedRecursive (box:Box, toys:Toy list, packing:Packing, cageBin:CageBin, maxIndexSoFar, discardZ, explicitPoint) =
  let accumulator = 0M<centimeters^3>
  match toys with
  | head :: tail ->
    let currentToyVolume = volume head.Dimensions.Length head.Dimensions.Width head.Dimensions.Height
    let packedToyVolume = getToysVolume(packing.Order.Toys, accumulator)
    let inclusiveToyVolume = packedToyVolume + currentToyVolume
    let index = calculateIndexViaListOfReferencePointsZValues(head, box, packing.SetOfReferencePoints, inclusiveToyVolume, maxIndexSoFar, cageBin, packing)
    let fitVolumePacking = doesBoxHaveEnoughFreeVolumeForToy(box.Dimensions, head, packing)
    let (_, indexPoint, _) = index
    let newCageBin = calculateCageBinWithCurrentToy(indexPoint, head, cageBin)
    let setOfReferencePointsFound = 
      [
        {
          X = indexPoint.X
          Y = indexPoint.Y
          Z = indexPoint.Z
        }
      ]
    let filteredReferencePoints = (Set fitVolumePacking.SetOfReferencePoints) - (Set setOfReferencePointsFound) |> Set.toList
    let newPackedToy =
      {
        Details = head.Details
        Dimensions = head.Dimensions
        Weight = head.Weight
        Origin = indexPoint
      }
    let availability = roomInBoxForNewToy(box.Dimensions, newPackedToy, packing)
    if availability then
        let newReferencePointsFromNewlyPackedToy = calculateToyReferencePoints(newPackedToy)
        let newReferencePoints = (Set filteredReferencePoints) + (Set newReferencePointsFromNewlyPackedToy) |> Set.toList
        let newOrder =
          {
            Details = fitVolumePacking.Order.Details
            OrderId = fitVolumePacking.Order.OrderId
            Toys = newPackedToy::fitVolumePacking.Order.Toys      
          }
        let newPacking =
          {       
            SetOfReferencePoints = newReferencePoints
            Order = newOrder
          }
        let newMaxIndexSoFar = (-1000M<centimeters^2>, indexPoint, newCageBin)
        packOrderInBoxWuDerivedRecursive (box, tail, newPacking, cageBin, newMaxIndexSoFar, discardZ, explicitPoint)
    else
        let newReferencePoints = (Set filteredReferencePoints) |> Set.toList
        let setNewPackedToyOutsideOfBox = 
          {
              Details = newPackedToy.Details                
              Dimensions = newPackedToy.Dimensions
              Weight = newPackedToy.Weight
              Origin =             
              { 
                  X = 0M<centimeters> - box.Dimensions.Length - 2M<centimeters>
                  Y = box.Dimensions.Width + 1M<centimeters>
                  Z = discardZ
              }
          } 
        let newOrder =
          {
            Details = fitVolumePacking.Order.Details
            OrderId = fitVolumePacking.Order.OrderId
            Toys = setNewPackedToyOutsideOfBox::fitVolumePacking.Order.Toys      
          }
        let newPacking =
          {       
            SetOfReferencePoints = newReferencePoints
            Order = newOrder
          }
        let newDiscardZ = discardZ + newPackedToy.Dimensions.Height
        packOrderInBoxWuDerivedRecursive (box, tail, newPacking, cageBin, maxIndexSoFar, newDiscardZ, explicitPoint)
    | [] -> packing

let packOrderInBoxWuScoped (box:Box, toys:Toy list, packing:Packing) =
  let initialCageBin =
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
  let maxIndexSoFar = (1000M</centimeters^2>, initialPoint, initialCageBin)
  let discardZ = 0M<centimeters>
  let newPacking = packOrderInBoxWuScopedRecursive(box, toys, packing, initialCageBin, maxIndexSoFar, discardZ, initialPoint)
  newPacking

let packOrderInBoxWuDerived (box:Box, toys:Toy list, packing:Packing) =
  let initialCageBin =
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
  let maxIndexSoFar = (1000M<centimeters^2>, initialPoint, initialCageBin)
  let discardZ = 0M<centimeters>
  let newPacking = packOrderInBoxWuDerivedRecursive(box, toys, packing, initialCageBin, maxIndexSoFar, discardZ, initialPoint)
  newPacking

let rec algorithmEvaluation (boxes:Box list, toys:Toy list, packing:Packing, cageBin:CageBin, maxIndexSoFar, algorithm:System.Func<Box * Toy list * Packing, Packing>) =
  let accumulator = 0M<centimeters^3>
  match boxes with
  | head :: tail ->
      let currentBoxVolume = volume head.Dimensions.Length head.Dimensions.Width head.Dimensions.Height
      let currentOrderVolume = getToysVolume(toys, accumulator)
      if (currentBoxVolume >= currentOrderVolume) then
        algorithm.Invoke(head, toys, packing)
      else
        algorithmEvaluation(tail, toys, packing, cageBin, maxIndexSoFar, algorithm)
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

let getCageBin (cageBin:CageBin) =
  match cageBin with 
  |  cageBin -> Success cageBin

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
      SetOfReferencePoints = initialSetOfReferencePoints
      Order = blankOrder
    }
  let box = getBox2 world
  let initialCageBin =
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
  let maxIndexSoFar = (0M</centimeters^2>, initialPoint, initialCageBin)
  fitnessEvaluation(world.Order.Toys, box, packing, initialCageBin, maxIndexSoFar, initialPoint)


let executeAlgorithmEvaluationWuDerived world =
  let packing =
    {       
      SetOfReferencePoints = initialSetOfReferencePoints
      Order = blankOrder
    }
  let box = getBox2 world
  let initialCageBin =
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
  let maxIndexSoFar = (-1000M<centimeters>, initialPoint, initialCageBin)
  let boxes = getBoxZ2 world
  algorithmEvaluation(boxes, world.Order.Toys, packing, initialCageBin, maxIndexSoFar, System.Func<Box * Toy list * Packing, Packing> packOrderInBoxWuDerived)

let executeAlgorithmEvaluationWu world =
  let packing =
    {       
      SetOfReferencePoints = initialSetOfReferencePoints
      Order = blankOrder
    }
  let box = getBox2 world
  let initialCageBin =
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
  let maxIndexSoFar = (1000M<centimeters>, initialPoint, initialCageBin)
  let boxes = getBoxZ2 world
  algorithmEvaluation(boxes, world.Order.Toys, packing, initialCageBin, maxIndexSoFar, System.Func<Box * Toy list * Packing, Packing> packOrderInBoxWuScoped)

let getToy world =
  match Some(world.Order.Toys |> Seq.head) with 
  |  Some toy -> Success toy
  |  None -> Failure "Toy does not exist"

let describeDetails details =
  printf "\n\n%s\n\n%s\n\n" details.Name details.Description
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
              Width = 2M<centimeters>
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
              Length = 5M<centimeters>
              Width = 7M<centimeters>
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
        {
          Details = 
            {
              Name = "Toy#5Name";
              Description = "Toy#5Description"
            };                 
          Dimensions = 
            {
              Length = 5M<centimeters>
              Width = 7M<centimeters>
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
          Length = 12M<centimeters>
          Width = 12M<centimeters>
          Height = 12M<centimeters>
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

let scale = 2.0

let convertBoxesToWebGL(box:Box, scene:Three.Scene, offset: decimal<centimeters>) = 
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
  box.position.x <- 0.0 + (scale * float (offset/1M<centimeters>))
  scene.add(box)

let calculateWebGLPositionFromWuCoordinates (boxDimension:decimal<centimeters>, toyPosition:decimal<centimeters>, toyDimension:decimal<centimeters>, scale: decimal) =
  let graphicCenter = 0.0M<centimeters>
  let halfTheBox = 0.5M * boxDimension * scale
  let halfTheToy = 0.5M * toyDimension * scale
  let graphicPosition = graphicCenter - halfTheBox + (toyPosition * scale) + halfTheToy
  Math.Round(graphicPosition/1M<centimeters>, 3)

let rec convertToysToWebGL(toys:Toy list, scene:Three.Scene, random:Random, box:Box, meshList:Three.Mesh list, offset: decimal<centimeters>) = 
  match toys with
  | head :: tail ->
      if head.Details.Description.Trim().Length > 0 then

        let mutable color = new Three.Color(255.0, 255.0, 255.0)  // White
        if tail.Length % 7 = 0 then
          color <- new Three.Color(255.0, 0.0, 0.0)               // Red
        else if tail.Length % 7 = 1 then       
          color <- new Three.Color(0.0, 255.0, 0.0)               // Green
        else if tail.Length % 7 = 2 then       
          color <- new Three.Color(0.0, 0.0, 255.0)               // Blue
        else if tail.Length % 7 = 3 then       
          color <- new Three.Color(255.0, 255.0, 0.0)             // Yellow
        else if tail.Length % 7 = 4 then       
          color <- new Three.Color(0.0, 255.0, 255.0)             // Light Blue
        else if tail.Length % 7 = 5 then       
          color <- new Three.Color(255.0, 0.0, 255.0)             // Purple

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

        toy.scale.set(scale, scale, scale) |> ignore
        toy.position.x <- float(calculateWebGLPositionFromWuCoordinates(box.Dimensions.Width, head.Origin.Y, head.Dimensions.Width, (decimal)scale))
                          + (scale * float (offset/1M<centimeters>))
        toy.position.y <- float(calculateWebGLPositionFromWuCoordinates(box.Dimensions.Height, head.Origin.Z, head.Dimensions.Height, (decimal)scale))
        toy.position.z <- float(calculateWebGLPositionFromWuCoordinates(box.Dimensions.Length, head.Origin.X, head.Dimensions.Length, (decimal)scale))
  
        scene.add(toy)


        convertToysToWebGL(tail, scene, random, box, toy::meshList, offset)
      else
        convertToysToWebGL(tail, scene, random, box, meshList, offset)
  | [] -> meshList

let convertAlgorithmToWebGl(algorithm:World, scene:Three.Scene, offset: decimal<centimeters>) =
  let boxes = getBoxZ2 algorithm
  let oneBox = (boxes |> Seq.head)
  convertBoxesToWebGL(oneBox, scene, offset)
  let random = new Random()
  //let toy = (algorithm.Order.Toys |> Seq.head)
  //let toys = [algorithm.Order.Toys.[2]]
  let helper = new Three.AxisHelper(30.0)
  scene.add(helper)
  let meshList = []
  let meshListReturn = convertToysToWebGL(algorithm.Order.Toys, scene, random, oneBox, meshList, offset)
  let mutable count = 1
  for mesh in meshListReturn do
    count <- count + 1
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
    camera.position.y <- 50.

    let packingWuDerived = executeAlgorithmEvaluationWuDerived algorithmWorld
    let algorithmWorldPackedWuDerived =
      {
        AvailableBoxes =
          allBoxes_WuExample3
          |> Seq.map (fun box -> (box.Id, box))
          |> Map.ofSeq
        Customer = customer
        Order = packingWuDerived.Order       
      }
    let boxes = getBoxZ2 algorithmWorldPackedWuDerived
    let oneBox = (boxes |> Seq.head)
    let offset1 = (oneBox.Dimensions.Length + 2M<centimeters>)
    convertAlgorithmToWebGl(algorithmWorldPackedWuDerived, scene, offset1) |> ignore

    let packingWu = executeAlgorithmEvaluationWu algorithmWorld
    let algorithmWorldPackedWu =
      {
        AvailableBoxes =
          allBoxes_WuExample3
          |> Seq.map (fun box -> (box.Id, box))
          |> Map.ofSeq
        Customer = customer
        Order = packingWu.Order       
      }
    let offset2 = 0M<centimeters> - (oneBox.Dimensions.Length + 2M<centimeters>)
    convertAlgorithmToWebGl(algorithmWorldPackedWu, scene, offset2) |> ignore    

    let onWindowResize(e:Browser.UIEvent):obj =
        camera.aspect <- getWidth() / getHeight()
        camera.updateProjectionMatrix()
        (renderer :> Three.Renderer).setSize(getWidth(), getHeight())
        controls.handleResize()
        null

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
