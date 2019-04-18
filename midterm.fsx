open System.Drawing

open System.Windows.Forms
open System.Drawing
open System.Drawing.Drawing2D

//Matrici di trasformazione
type WVMatrix () =
  let wv = new Drawing2D.Matrix()
  let vw = new Drawing2D.Matrix()
    
  member this.TranslateW (tx, ty) =
    wv.Translate(tx, ty)
    vw.Translate(-tx, -ty, Drawing2D.MatrixOrder.Append)

  member this.ScaleW (sx, sy) =
    wv.Scale(sx, sy)
    vw.Scale(1.f /sx, 1.f/ sy, Drawing2D.MatrixOrder.Append)

  member this.RotateW (a) =
    wv.Rotate(a)
    vw.Rotate(-a, Drawing2D.MatrixOrder.Append)

  member this.RotateV (a) =
    vw.Rotate(a)
    wv.Rotate(-a, Drawing2D.MatrixOrder.Append)

  member this.TranslateV (tx, ty) =
    vw.Translate(tx, ty)
    wv.Translate(-tx, -ty, Drawing2D.MatrixOrder.Append)

  member this.ScaleV (sx, sy) =
    vw.Scale(sx, sy)
    wv.Scale(1.f /sx, 1.f/ sy, Drawing2D.MatrixOrder.Append)

  member this.TransformPointV (p:PointF) =
    let a = [| p |]
    vw.TransformPoints(a)
    a.[0]

  member this.TransformPointW (p:PointF) =
    let a = [| p |]
    wv.TransformPoints(a)
    a.[0]

  member this.VW 
    with get() = vw
  member this.WV 
    with get() = wv
    
type Graph() =   
   let mutable nodes = []  
   let mutable edges = []  
   
   let mutable sequence = 0

   let stack = NodeStack() 
   let visited = VisitedList() 
   let visit node = 
     visited.Add(node) 
   let (| Visited | _ |) node = 
     if visited.Contains(node) then
       Some node
     else
       None
   
   member this.Visited
     with get()  = visited
   
   member this.Sequence 
     with get()  = sequence
     and  set(v) = sequence <- v
   
   member this.Nodes with get() = nodes  
   member this.Edges with get() = edges
     
   member this.CreateNode(id) =   
     match this.FindNode(id) with  
       | Some(n) -> None  
       | None ->   
         let node = Node(this, ID=id)  
         nodes <- nodes @ [ node ]  
         Some node
       
   member this.CreateEdgeFromNode(from:Node, ``to``:Node, id) =   
     match this.FindEdge id with  
     | Some(edge) -> None  
     | None ->   
       let edge = Edge(this, from, ``to``, ID=id)
       //Il grafo in questo modo sarà non orientato
       from.AddOutgoingEdge(edge)  
       from.AddIncomingEdge(edge)
       ``to``.AddIncomingEdge(edge)  
       ``to``.AddOutgoingEdge(edge)
       edges <- edges @ [edge]  
       Some edge  

       
//   member this.CreateEdgeFromNode(from:Node, ``to``:Node, id) =   
//     match this.FindEdge id with  
//     | Some(edge) -> None  
//     | None ->   
//       let edge = Edge(this, from, ``to``, ID=id)  
//       from.AddOutgoingEdge(edge)  
//       ``to``.AddIncomingEdge(edge)  
//       edges <- edges @ [edge]  
//       Some edge  

   member this.CreateEdgeFromID(from, ``to``, id) =   
     let fromNode = this.FindNode(from)  
     let toNode = this.FindNode(``to``)  
     match fromNode, toNode with  
       | Some(n0), Some(n1) -> 
         //Controllo se sono già collegati con un Edge
         match((edges:Edge list) |> Seq.tryFind(fun edge ->
               (edge.FromNode = n0 &&
                edge.ToNode = n1      ) 
            ||
               (edge.FromNode = n1 &&
                edge.ToNode = n0)     ))
           with 
           | None -> this.CreateEdgeFromNode(n0, n1, id)
           | _ -> None
           
       | _ -> None  
         
//   member this.CreateEdgeFromID(from, ``to``, id) =   
//     let fromNode = this.FindNode(from)  
//     let toNode = this.FindNode(``to``)  
//     match fromNode, toNode with  
//       | Some(n0), Some(n1) -> this.CreateEdgeFromNode(n0, n1, id)  
//       | _ -> None  

   member this.FindNode(id) =   
     (nodes:Node list) |> Seq.tryFind(fun n -> n.ID = id)  
   member this.FindEdge(id) =   
     (edges:Edge list) |> Seq.tryFind(fun edge -> edge.ID = id)  

   member this.RemoveEdge(edge:Edge) =   
     (edge.FromNode:Node).RemoveOutgoingEdge(edge)  
     (edge.ToNode:Node).RemoveIncomingEdge(edge)  
     edges <- edges |> List.filter (fun n -> n<>edge)  
   member this.RemoveNode(node:Node) =   
     node.OutgoingEdges @ node.IncomingEdges |> List.iter this.RemoveEdge  
     nodes <- nodes |> List.filter (fun n -> n<>node)
    
   member this.dfsGraph (start:Node)= 
     stack.Push(start) 
     visited.Clear()  
     while stack.Count <> 0 do
     let currentNode = stack.Pop() 
     match currentNode with 
     | Visited(_) -> () 
     | _ -> 
       visit currentNode 
       let nodes = 
         currentNode.OutgoingEdges 
         |> Seq.map (fun edge -> edge.ToNode) 
         |> Seq.distinctBy (fun node -> node.ID)
         |> Seq.toList
       nodes
       |> Seq.iter (fun node -> stack.Push node)
       let nodes2 = 
         currentNode.IncomingEdges 
         |> Seq.map (fun edge -> edge.FromNode) 
         |> Seq.distinctBy (fun node -> node.ID)
         |> Seq.toList
       nodes2
       |> Seq.iter (fun node -> stack.Push node)
     
 and Node(g) =  
   let mutable incomingEdges = []  
   let mutable outgoingEdges = []
   
   let mutable rettangolo = RectangleF()  
   
   let mutable label = ""
   let mutable fontSize = 1
   let mutable fontText = new Font("Consolas", fontSize |> single)
   let mutable brushEllipse = new SolidBrush(Color.Teal)
   
   let mutable dx = 0.f
   let mutable dy = 0.f
   
   member this.Dx
     with get()  = dx
     and  set(v) = dx <- v
   
   member this.Dy
     with get()  = dy
     and  set(v) = dy <- v
   
   member this.Rettangolo
     with get()  = rettangolo
     and  set(v) = rettangolo <- v
   
   member this.Height
     with get()  = rettangolo.Height
     and  set(v) = rettangolo.Height <- v
    
   member this.Width
     with get()  = rettangolo.Width
     and  set(v) = rettangolo.Width <- v
    
   member this.X
     with get()  = rettangolo.X
     and  set(v) = rettangolo <- RectangleF(v, rettangolo.Y, rettangolo.Width, rettangolo.Height)
    
   member this.Y
     with get()  = rettangolo.Y
     and  set(v) = rettangolo <- RectangleF(rettangolo.X, v, rettangolo.Width, rettangolo.Height) 
    
   member this.FontSize
     with get() = fontSize
   
   member this.Label
     with get()  = label
     and  set(v) = 
       label <- v
       fontSize <- 22 - v.Length
       fontText <- new Font("Consolas", fontSize |> single)
     
   member this.FontText
     with get()  = fontText
     and  set(v) = fontText <- v 
   
   member this.BrushEllipse
     with get()  = brushEllipse
     and  set(v) = brushEllipse <- v
   
   member this.Contains(x, y) = 
     let square v = v * v
     let x1 = x - (this.X + (this.Width / 2.f))
     let y1 = y - (this.Y + (this.Height / 2.f))
     square x1 + square y1 <= square (this.Width / 2.f)
     
   member this.OnPaint(g : Graphics, wv : WVMatrix) =
   
     let t = g.Transform
     g.Transform <- wv.WV 
   
     let pen = Pens.Black
     g.FillEllipse(this.BrushEllipse, this.Rettangolo)
     g.DrawEllipse(pen, this.Rettangolo)
    
     let textSize = g.MeasureString(this.Label, this.FontText)
     let brushText = new SolidBrush(Color.White)
     
     g.DrawString(this.Label, this.FontText, brushText, 
      (this.X) + (this.Width / 2.f) - (textSize.Width / 2.f), 
      (this.Y) + (this.Height / 2.f) - (textSize.Height / 2.f))

     g.Transform <- t
     
   member val ID = Unchecked.defaultof<_> with get, set  
   member val Data = Unchecked.defaultof<_> with get, set  
   member this.IncomingEdges with get() = incomingEdges  
   member this.OutgoingEdges with get() = outgoingEdges  
   member this.AddIncomingEdge(edge:Edge) =   
     if edge.ToNode = this then  
       incomingEdges <- incomingEdges |> List.append [edge]  
   member this.AddOutgoingEdge(edge:Edge) =   
     if edge.FromNode = this then  
       outgoingEdges <- outgoingEdges |> List.append [edge]  
   member this.RemoveIncomingEdge(edge:Edge) =   
     incomingEdges <- incomingEdges |> List.filter (fun n -> n<>edge)  
   member this.RemoveOutgoingEdge(edge:Edge) =   
     outgoingEdges <- outgoingEdges |> List.filter (fun n -> n<> edge)  
   override this.ToString() =  
     sprintf "Node(%A)" this.ID  
 and Edge(g, from:Node, ``to``:Node) = 
 
   let mutable label = ""
   let mutable fontSize = 1
   let mutable fontText = new Font("Consolas", fontSize |> single)
 
   let circleDiameterToRemoveEdge = 20.f
   
   member this.CircleDiameterToRemoveEdge
     with get() = circleDiameterToRemoveEdge
 
   member this.FontSize
     with get() = fontSize
    
   member this.Label
     with get()  = label
     and  set(v) = 
       label <- v
       fontSize <- 22 - v.Length
       fontText <- new Font("Consolas", fontSize |> single)
      
   member this.FontText
     with get()  = fontText
     and  set(v) = fontText <- v 

   member this.Contains(x, y) = 
     let centerFromX = this.FromNode.X + (this.FromNode.Width / 2.f)
     let centerFromY = this.FromNode.Y + (this.FromNode.Height / 2.f)
    
     let centerToX = this.ToNode.X + (this.ToNode.Width / 2.f)
     let centerToY = this.ToNode.Y + (this.ToNode.Height / 2.f)
     
     let square v = v * v
     let x1 = x - (centerToX + (centerFromX - centerToX) * 0.5f)
     let y1 = y - (centerToY + (centerFromY - centerToY) * 0.5f)
     square x1 + square y1 <= square (this.CircleDiameterToRemoveEdge / 2.f)
     
   member this.OnPaint(g : Graphics , wv : WVMatrix, action : string option) = 
   
     let t = g.Transform
     g.Transform <- wv.WV 
     
     let pen = new Pen(Brushes.Blue)
     pen.Width <- 1.f
     pen.Alignment <- PenAlignment.Center;
     
     let centerFromX = this.FromNode.X + (this.FromNode.Width / 2.f)
     let centerFromY = this.FromNode.Y + (this.FromNode.Height / 2.f)
     
     let centerToX = this.ToNode.X + (this.ToNode.Width / 2.f)
     let centerToY = this.ToNode.Y + (this.ToNode.Height / 2.f)
     
     g.DrawLine(pen, 
       PointF(centerFromX, centerFromY), 
       PointF(centerToX, centerToY))
     
     match action with
     | Some "Delete Edge" ->
         let brushEllipse = new SolidBrush(Color.OrangeRed)
         g.FillEllipse(brushEllipse, centerToX + (centerFromX - centerToX) * 0.5f - this.CircleDiameterToRemoveEdge / 2.f, 
                            centerToY + (centerFromY - centerToY) * 0.5f - this.CircleDiameterToRemoveEdge / 2.f,
                            this.CircleDiameterToRemoveEdge, this.CircleDiameterToRemoveEdge)
     | _ -> ()
     
     let brushText = new SolidBrush(Color.Black)
     let textSize = g.MeasureString(this.Label, this.FontText)
     g.DrawString(this.Label, this.FontText, brushText, 
       PointF( centerToX + (centerFromX - centerToX) * 0.5f - (textSize.Width / 2.f) * 1.5f, 
               centerToY + (centerFromY - centerToY) * 0.5f - (textSize.Height / 2.f) * 1.5f))
   
     g.Transform <- t
     
   member val ID = Unchecked.defaultof<_> with get, set  
   member val Data = Unchecked.defaultof<_> with get, set  
   member this.FromNode with get() = from  
   member this.ToNode with get() = ``to``  
   override this.ToString() =   
     sprintf "Edge(%A, %A -> %A)" this.ID this.FromNode this.ToNode
 and NodeStack = System.Collections.Generic.Stack<Node>
 and VisitedList = System.Collections.Generic.List<Node>

type Button(r : Rectangle) = 

  let mutable rettangolo = r
  
  let mutable label = ""
  let fontSize = 14
  let mutable fontText = new Font("Arial", fontSize |> single)
   
  member this.Label
    with get()  = label
    and  set(v) = label <- v
      
  member this.FontText
    with get()  = fontText
    and  set(v) = fontText <- v 

  member this.Rettangolo
    with get()  = rettangolo
    and  set(v) = rettangolo <- v
     
  member this.Height
    with get()  = rettangolo.Height
    and  set(v) = rettangolo.Height <- v
  
  member this.Width
    with get()  = rettangolo.Width
    and  set(v) = rettangolo.Width <- v
  
  member this.X
    with get()  = rettangolo.X
    and  set(v) = rettangolo <- Rectangle(v, rettangolo.Y, rettangolo.Width, rettangolo.Height)
  
  member this.Y
    with get()  = rettangolo.Y
    and  set(v) = rettangolo <- Rectangle(rettangolo.X, v, rettangolo.Width, rettangolo.Height) 
     
  member this.Contains(x, y) = 
    (this.X <= x) && 
    (this.X + this.Width >= x) &&
    (this.Y <= y) &&
    (this.Y + this.Height >= y)
     
  member this.OnPaint(g : Graphics) = 
    let pen = Pens.Black
    let brushRectangle = new SolidBrush(Color.LimeGreen)
    g.FillRectangle(brushRectangle, this.Rettangolo)
    g.DrawRectangle(pen, this.Rettangolo)
    
    let brushText = new SolidBrush(Color.White)
    let textSize = g.MeasureString(this.Label, this.FontText)
    g.DrawString(this.Label, this.FontText, brushText, 
      PointF( (this.X |> single) + (this.Width / 2 |> single) - (textSize.Width / 2.f), 
              (this.Y |> single) + (this.Height / 2 |> single) - (textSize.Height / 2.f)))
              
type MidControl() as this = 
 inherit UserControl()
 do
   this.SetStyle(ControlStyles.OptimizedDoubleBuffer, true)
   this.SetStyle(ControlStyles.AllPaintingInWmPaint, true)
 
 //Grafo
 let graph = Graph()
 
 //Azione in corso
 let mutable action = ""
 
 //Nodo temporaneo
 let mutable tempNode = None
 
 //Array di bottoni
 let arrayButtons = ResizeArray<Button>()
 
 //New node
 let buttonNewNode = Button(Rectangle(130, 0, 100, 29))
 //New edge
 let buttonNewEdge = Button(Rectangle(230, 0, 100, 29))
 //Delete node
 let buttonDeleteNode = Button(Rectangle(330, 0, 130, 29))
 //Delete edge
 let buttonDeleteEdge = Button(Rectangle(460, 0, 130, 29))
 //Rotate Clockwise
 let buttonRotateClockwise = Button(Rectangle(600, 0, 40, 29))
 //Rotate Counterclockwise
 let buttonRotateCounterclockwise = Button(Rectangle(650, 0, 40, 29))
 //ZoomIn
 let buttonZoomIn = Button(Rectangle(700, 0, 40, 29))
 //ZoomOut
 let buttonZoomOut = Button(Rectangle(750, 0, 40, 29))
 //Left
 let buttonLeft = Button(Rectangle(800, 29, 40, 29))
 //Up
 let buttonUp = Button(Rectangle(850, 0, 40, 29))
 //Right
 let buttonRight = Button(Rectangle(900, 29, 40, 29))
 //Down
 let buttonDown = Button(Rectangle(850, 29 * 2, 40, 29))
 
 //Free Hand
 let buttonFreeHand = Button(Rectangle(845, 31, 50, 25))
 
 //World View Matrix inizializzata con la matrice identica
 let wv = WVMatrix()
 
 //Dice se e quale nodo stiamo spostando
 let mutable dragging = None
 
 //Dice l'azione che stiamo facendo
 let mutable action = None
 
 //Switch Free Hand, determina lo spostamento dei nodi
 let mutable freeHand = None
 
 //Timer per tenere premuto un pulsante
 let timer = new Timer(Interval = 16)
 
 //Timer per gli spostamenti
 let timerMovements = new Timer(Interval = 10)
 
 do
   timerMovements.Start()
   buttonNewNode.Label <- "New Node"
   buttonNewEdge.Label <- "New Edge"
   buttonDeleteNode.Label <- "Delete Node"
   buttonDeleteEdge.Label <- "Delete Edge"
   buttonLeft.Label <- "←"
   buttonUp.Label <- "↑"
   buttonRight.Label <- "→"
   buttonDown.Label <- "↓"
   buttonRotateClockwise.Label <- "⭮"
   buttonRotateCounterclockwise.Label <- "⭯"
   buttonZoomIn.Label <- "+"
   buttonZoomOut.Label <- "−"
   buttonFreeHand.Label <- "Free"
   
   arrayButtons.Add(buttonNewNode)
   arrayButtons.Add(buttonNewEdge)
   arrayButtons.Add(buttonDeleteNode)
   arrayButtons.Add(buttonDeleteEdge)
   arrayButtons.Add(buttonLeft)
   arrayButtons.Add(buttonUp)
   arrayButtons.Add(buttonRight)
   arrayButtons.Add(buttonDown)
   arrayButtons.Add(buttonRotateClockwise)
   arrayButtons.Add(buttonRotateCounterclockwise)
   arrayButtons.Add(buttonZoomIn)
   arrayButtons.Add(buttonZoomOut)
   arrayButtons.Add(buttonFreeHand)
   
   timer.Tick.Add(fun _ -> 
     this.Update()
   )
   
   timerMovements.Tick.Add(fun _ ->
     graph.Nodes |> Seq.iter (fun node ->
       let changeX = node.Dx / 2.f
       let changeY = node.Dy / 2.f
       node.X <- node.X + changeX
       node.Y <- node.Y + changeY
       node.Dx <- changeX
       node.Dy <- changeY
       this.Invalidate()
     )
   )
 
 //Textbox
 let mutable textbox = new TextBox()
 
 member this.TextBox
   with get()  = textbox
   and  set(v) = textbox <- v 
 
 member this.Graph
   with get()  = graph
 
 
 member this.Update() = 
   match action with 
   | Some "←" -> 
       wv.TranslateV (-10.f, 0.f)
       this.Invalidate()
   | Some "↑" -> 
       wv.TranslateV (0.f, -10.f)
       this.Invalidate()
   | Some "→" -> 
       wv.TranslateV (10.f, 0.f)
       this.Invalidate()
   | Some "↓" -> 
       wv.TranslateV (0.f, 10.f)
       this.Invalidate()
   | Some "⭮" -> 
       let client = this.ClientSize
       wv.TranslateV(client.Width / 2 |> single, client.Height / 2 |> single)
       wv.RotateV -10.f           
       wv.TranslateV(-client.Width / 2 |> single, -client.Height / 2 |> single)
       this.Invalidate()
   | Some "⭯" -> 
       let client = this.ClientSize
       wv.TranslateV(client.Width / 2 |> single, client.Height / 2 |> single)
       wv.RotateV 10.f
       wv.TranslateV(-client.Width / 2 |> single, -client.Height / 2 |> single)
       this.Invalidate()
   | _ -> ()
 
 override this.OnPaint e =
   let g = e.Graphics
   g.SmoothingMode <- System.Drawing.Drawing2D.SmoothingMode.AntiAlias
   
   graph.Edges |> Seq.iter (fun edge ->
     edge.OnPaint(g, wv, action)
     ()
   )
   graph.Nodes |> Seq.iter (fun node ->
     node.OnPaint(g, wv)
   )
   
   arrayButtons |> Seq.iter(fun button ->
     button.OnPaint(g)
   )

 override this.OnMouseDown e =
   //Controllo se ho fatto click su un pulsante
   let button = arrayButtons |> Seq.tryFindBack(fun button ->   
     button.Contains(e.X, e.Y)
   )
   match button with
   | Some button_ ->
       match button_.Label with 
       | "New Node" -> 
           action <- Some button_.Label
       | "New Edge" -> 
           action <- Some button_.Label
       | "Delete Node" -> 
           action <- Some button_.Label
       | "Delete Edge" -> 
           action <- Some button_.Label
           this.Invalidate()
       | "←" -> 
           action <- Some button_.Label
           timer.Start()
       | "↑" -> 
           action <- Some button_.Label
           timer.Start()
       | "→" -> 
           action <- Some button_.Label
           timer.Start()
       | "↓" -> 
           action <- Some button_.Label
           timer.Start()
       | "⭮" -> 
           action <- Some button_.Label
           timer.Start()
       | "⭯" -> 
           action <- Some button_.Label
           timer.Start()
       | "+" -> 
           action <- Some button_.Label
       | "−" -> 
           action <- Some button_.Label
       | "Free" ->
           match freeHand with
           | None -> 
               freeHand <- Some true
           | Some _ -> 
               freeHand <- None
       | _ -> ()
       //A prescindere, dato che hai cliccato su un pulsante, annulla,
       //se attiva, l'operazione di addEdge
       match tempNode with
       | None -> ()
       | Some (node : Node) -> 
           node.BrushEllipse <- new SolidBrush(Color.Teal)
   | _ -> //non ho fatto click sul bottone, controllo i nodi 
   
   let node = graph.Nodes |> Seq.tryFindBack(fun node ->
   
     let mousePos = PointF(e.X |> single, e.Y |> single) |> wv.TransformPointV
     node.Contains(mousePos.X |> single, mousePos.Y |> single)
   )
   match node with 
   | Some node_ -> 
       
       match action with
       | Some "New Edge" -> 
           node_.BrushEllipse <- new SolidBrush(Color.BlueViolet)
           tempNode <- Some node_
           this.Invalidate()
           action <- Some "New Edge Step2"
       | Some "New Edge Step2" ->
           tempNode.Value.BrushEllipse <- new SolidBrush(Color.Teal)
           let newEdge = graph.CreateEdgeFromID(tempNode.Value.ID, node_.ID, graph.Sequence)
           match newEdge with
           | Some edge -> 
               graph.Sequence <- graph.Sequence + 1
               newEdge.Value.Label <- this.TextBox.Text
           | _ -> ()
           tempNode <- None
           this.Invalidate()
           action <- None
       | Some "Delete Node" -> 
           graph.RemoveNode(node_)
           this.Invalidate()
           action <- None
       | _ -> 
         //Eseguo la DFS per spostare tutti i nodi collegati
         let startNode = node_
         graph.dfsGraph startNode
         
         let nodesToDrag = 
           graph.Visited
           |> Seq.toList
           |> Seq.filter(fun node -> node.ID <> startNode.ID)
         let mousePos = PointF(e.X |> single, e.Y |> single) |> wv.TransformPointV
         
         let dx = (mousePos.X |> single) - node_.X
         let dy = (mousePos.Y |> single) - node_.Y
         dragging <- Some(startNode, nodesToDrag, dx, dy)
   | _ -> //non ho fatto click sul nodo,
   
   let edge = graph.Edges |> Seq.tryFindBack(fun edge ->
  
     let mousePos = PointF(e.X |> single, e.Y |> single) |> wv.TransformPointV
     edge.Contains(mousePos.X |> single, mousePos.Y |> single)
   )
   match edge with
   | Some edge_ -> 
       graph.RemoveEdge(edge_)
       this.Invalidate()
       action <- None
   | _ -> //non ho fatto click su un punto di eliminazione Edge,   
          //controllo se c'è un'azione in corso

   match action with
   | Some "New Node" -> 
       let diameter = 100.f
       let mousePos = PointF(e.X |> single, e.Y |> single) |> wv.TransformPointV
       let newNode = graph.CreateNode(graph.Sequence)
       match newNode with
       | Some node -> 
           graph.Sequence <- graph.Sequence + 1
           newNode.Value.Rettangolo <- 
            RectangleF(mousePos.X - diameter / 2.f |> single, 
                       mousePos.Y - diameter / 2.f |> single, diameter, diameter)
           newNode.Value.Label <- this.TextBox.Text
           this.Invalidate()
       | _ -> ()
       action <- None
       
   | Some "New Edge" -> 
       ()
   | Some "Delete Node" -> 
       ()
   | Some "Delete Edge" -> 
       ()
   | Some "+" -> 
       let mousePos = PointF(e.X |> single, e.Y |> single) |> wv.TransformPointV
       wv.TranslateV(mousePos.X, mousePos.Y)
       wv.ScaleV(1.f/1.1f, 1.f/1.1f)
       wv.TranslateV(-mousePos.X, -mousePos.Y)
       let client = this.ClientSize
       wv.TranslateV(e.X - client.Width / 2 |> single, e.Y - (client.Height / 2) |> single)
       this.Invalidate()
       action <- None
   | Some "−" -> 
       let mousePos = PointF(e.X |> single, e.Y |> single) |> wv.TransformPointV
       wv.TranslateV(mousePos.X, mousePos.Y)
       wv.ScaleV(1.1f, 1.1f)
       wv.TranslateV(-mousePos.X, -mousePos.Y)
       let client = this.ClientSize
       wv.TranslateV(e.X - client.Width / 2 |> single, e.Y - (client.Height / 2) |> single)
       this.Invalidate()
       action <- None
   | _ -> ()
   
      
 override this.OnMouseMove e =
   match dragging with
   | Some(startNode, nodesToDrag, dx, dy) ->
   
       let saveX = startNode.X
       let saveY = startNode.Y
       
       let mousePos = PointF(e.X |> single, e.Y |> single) |> wv.TransformPointV
       startNode.X <- (mousePos.X |> single) - dx
       startNode.Y <- (mousePos.Y |> single) - dy
       
       match freeHand with
       | None -> 
           let dx = startNode.X - saveX
           let dy = startNode.Y - saveY
           
           nodesToDrag
           |> Seq.iter (fun node_ -> 
// Spostamento rigido
//                node_.X <- node_.X + dx
//                node_.Y <- node_.Y + dy
                node_.Dx <- node_.Dx + dx
                node_.Dy <- node_.Dy + dy
           )
       | _ -> ()
      
       this.Invalidate()
   | _ -> ()
   
 override this.OnMouseUp e =
 
   match action with
   | Some "←" -> 
       timer.Stop()
       action <- None
   | Some "↑" -> 
       timer.Stop()
       action <- None
   | Some "→" -> 
       timer.Stop()
       action <- None
   | Some "↓" -> 
       timer.Stop()
       action <- None
   | Some "⭮" -> 
       timer.Stop()
       action <- None
   | Some "⭯" -> 
       timer.Stop()
       action <- None
   | _ -> ()
 
   dragging <- None
 
 override this.OnResize e =
   this.Invalidate()

let form = new Form(Text = "Midterm", TopMost = false, Width = 1024, Height = 768)
let textbox = new TextBox(Width = 130, Height = 30, Left = 0, MaxLength = 10, 
                          AutoSize = false, Font = new Font("Arial", 14.f))
let midControl = new MidControl(Dock = DockStyle.Fill)
midControl.TextBox <- textbox
let graph = midControl.Graph

form.Controls.Add(textbox)
form.Controls.Add(midControl)
midControl.Select()
form.Show()