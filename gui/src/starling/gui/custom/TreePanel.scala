package starling.gui.custom

import java.awt.{Graphics2D, Color, Cursor}
import swing.event._
import swing.Swing._
import starling.pivot.view.swing.{FixedImagePanel, MigPanel}
import swing._
import starling.gui.GuiUtils._
import starling.pivot.controller.{TreePivotFilterNode, TreePivotFilter}
import javax.swing.tree.{DefaultTreeModel, TreeModel, TreeCellRenderer, DefaultMutableTreeNode}
import collection.mutable.{ListBuffer, HashMap}
import starling.gui.{GuiUtils, StarlingIcons}
import javax.swing.{ToolTipManager, UIManager, JTree}
import starling.pivot._

// The selected is a var rather than using the copy method as we need the objects to stay the same for filtering.
case class CheckBoxListElement(var selected:Boolean, value:Any, label:String, var enabled:Boolean)

object TreePanel {
  val CheckBoxBorder = EmptyBorder(2, 2, 2, 0)
  val CheckBoxWidth = new CheckBox("") {
    border = CheckBoxBorder
  }.preferredSize.width
}

case class TreePanel(initialValuesAndSelection:(TreePivotFilter, Selection),
                     showOther:Boolean=false, transforms:Option[FilterWithOtherTransform]=None)
        extends MigPanel("insets 0", "[fill]", "[p]2lp[fill]2lp[p]") {
  val filterPanel = new TreePanelFilterPanel
  val treeComponent = new Tree
  ToolTipManager.sharedInstance.registerComponent(treeComponent.peer)
  val scrollPane = new ScrollPane(treeComponent)
  val buttonPanel = new TreePanelButtonPanel

  add(filterPanel, "pushx, growx, wrap")
  add(scrollPane, "push, grow, wrap")
  add(buttonPanel, "growx")

  reactions += {
    case DownPressedFromFilterAreaEvent => {
      if (treeComponent.peer.getRowCount > 0) {
        treeComponent.requestFocusInWindow
        treeComponent.peer.setSelectionRow(0)
      }
    }
    case FilterPopupEvent(filterText) => filterPopup(filterText)
    case CancelEvent(`buttonPanel`) => hidePopup
    case GenerateFromSelectionEvent => generateFromSelection
    case SelectFiltered(filterText) => selectFiltered(filterText)

    case KeyPressed(`treeComponent`, scala.swing.event.Key.Enter, _, _) => generateFromSelection
    case KeyPressed(`treeComponent`, scala.swing.event.Key.Space, _, _) => {
      // For any selected rows, enable or disable them.
      val selectionPaths = treeComponent.peer.getSelectionPaths
      if (selectionPaths != null) {
        val selectedItems = selectionPaths.map(_.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode])
        selectItems(selectedItems.toList.filter(_.getUserObject.asInstanceOf[CheckBoxListElement].enabled))
      }
    }
    case MouseClicked(`treeComponent`, point, _, 2, _) => {
      // When a user double clicks on a list item check area, select that item and deselect all others.
      val path = treeComponent.peer.getPathForLocation(point.x, point.y)
      if (path != null) {
        val node = path.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]
        val element = node.getUserObject.asInstanceOf[CheckBoxListElement]
        if (element.enabled) {
          if (element.value != OtherValue) {
            filterHelper.setOtherValueEnabled(true)
            selectSingleItem(node)
          }
        }
      }
    }
    case MouseClicked(`treeComponent`, point, keyModifiers, _, _) => {
      import swing.event.Key.Modifier._
      val shiftDown = ((keyModifiers & Shift) == Shift)
      val controlDown = ((keyModifiers & Control) == Control)
      if (!shiftDown && !controlDown) {
        val path = treeComponent.peer.getPathForLocation(point.x, point.y)
        // path can be null if you click outside of the actual leaf/node.
        if (path != null) {
          val selectedValue = path.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]
          val element = selectedValue.getUserObject.asInstanceOf[CheckBoxListElement]
          if (element.enabled) {
            val level = selectedValue.getLevel
            // If you click on the tick, you want to select without going to the next page, but if you click anywhere else, you want to go to the
            // next page with the item you clicked on selected.
            if (point.x < (level * treeComponent.magicNumberForTreeSize + TreePanel.CheckBoxWidth)) {
              selectItems(List(selectedValue))
            } else {
              if (element.value != OtherValue) {
                filterHelper.setOtherValueEnabled(false)
                selectSingleItem(selectedValue)
                generateFromSelection
              }
            }
          }
        }
      }
    }
    case MouseMoved(`treeComponent`, point, _) => {
      val index = treeComponent.peer.getRowForLocation(point.x, point.y)
      if (index != treeComponent.treeCellRenderer.mouseOverRow) {
        treeComponent.treeCellRenderer.mouseOverRow = index
        treeComponent.repaint
      }
    }
    case MouseExited(`treeComponent`, _, _) => {treeComponent.treeCellRenderer.mouseOverRow = -1; treeComponent.repaint}
  }
  listenTo(filterPanel, buttonPanel, treeComponent.keys, treeComponent.mouse.clicks, treeComponent.mouse.moves)

  val filterHelper = FilterHelper(treeComponent, showOther)
  filterHelper.resetPopup(initialValuesAndSelection, transforms)
  makeTreeValid(selectedNodes)

  private def hidePopup = {
    publish(CancelEvent(this))
  }

  def scrollToFirstSelectedNode {
    val root = treeComponent.rootNode
    var found = false
    var row = 0
    var firstSelectedNode:DefaultMutableTreeNode = null
    def recurse(node:DefaultMutableTreeNode) {
      if (node.getUserObject.asInstanceOf[CheckBoxListElement].selected) {
        found = true
        firstSelectedNode = node
      }
      if (!found) {
        row += 1
        val enum = node.children
        while (enum.hasMoreElements) {
          recurse(enum.nextElement.asInstanceOf[DefaultMutableTreeNode])
        }
      }
    }
    recurse(root)
    if (found) {
      val rowToUse = if (row - 1 >= 0) {
        row - 1
      } else {
        row
      }
      treeComponent.peer.scrollRowToVisible(rowToUse)
    }
  }

  def selectSingleItem(nodeToSelect:DefaultMutableTreeNode) {
    // Go over the entire tree and deselect everything apart from the node passed in.
    recurse(treeComponent.rootNode)
    def recurse(node:DefaultMutableTreeNode) {
      if (node == nodeToSelect) {
        node.getUserObject.asInstanceOf[CheckBoxListElement].selected = true
      } else {
        node.getUserObject.asInstanceOf[CheckBoxListElement].selected = false
      }
      val enum = node.children
      while (enum.hasMoreElements) {
        recurse(enum.nextElement.asInstanceOf[DefaultMutableTreeNode])
      }
    }
    // Ensure the children of the selected node are selected as well.
    recurse2(nodeToSelect)
    def recurse2(node:DefaultMutableTreeNode) {
      node.getUserObject.asInstanceOf[CheckBoxListElement].selected = true
      val enum = node.children
      while (enum.hasMoreElements) {
        recurse2(enum.nextElement.asInstanceOf[DefaultMutableTreeNode])
      }
    }
    treeComponent.repaint()
  }

  def selectItems(selectedItems:List[DefaultMutableTreeNode]) {
    // As the model can be filtered, I need to get the nodes from the unfiltered tree.
    val selectedItemsCBLE = selectedItems.map(_.getUserObject.asInstanceOf[CheckBoxListElement]).toSet
    val unfilteredSelectedItems = new ListBuffer[DefaultMutableTreeNode]
    val unfilteredRootNode = treeComponent.rootNode
    def unfilteredRecurse(node:DefaultMutableTreeNode) {
      if (!node.isLeaf) {
        val enum = node.children
        while (enum.hasMoreElements) {
          unfilteredRecurse(enum.nextElement.asInstanceOf[DefaultMutableTreeNode])
        }
      }
      val cble = node.getUserObject.asInstanceOf[CheckBoxListElement]
      if (selectedItemsCBLE.contains(cble)) {
        unfilteredSelectedItems += node
      }
    }
    unfilteredRecurse(unfilteredRootNode)
    val selItems = unfilteredSelectedItems.toList

    for (selectedItem <- selItems) {
      val currentValue = selectedItem.getUserObject.asInstanceOf[CheckBoxListElement]
      currentValue.selected = !currentValue.selected
    }

    makeTreeValid(selItems)
    treeComponent.repaint
  }

  /**
   * This goes through the tree and selects on the nodes and leaves that match the filter text. If the matched nodes have children, they are also
   * selected. If all children of a node are selected, that node is selected (even if it doesn't match the text).
   */
  private def selectFiltered(text:String) {
    val testText = text.trim.toLowerCase
    val selectedItems = new ListBuffer[DefaultMutableTreeNode]
    def recurse(node:DefaultMutableTreeNode) {
      val userObject = node.getUserObject.asInstanceOf[CheckBoxListElement]
      if (userObject.label.trim.toLowerCase.contains(testText)) {
        node.getUserObject.asInstanceOf[CheckBoxListElement].selected = true
        selectedItems += node
      } else {
        node.getUserObject.asInstanceOf[CheckBoxListElement].selected = false
      }
      val enum = node.children
      while (enum.hasMoreElements) {
        recurse(enum.nextElement.asInstanceOf[DefaultMutableTreeNode])
      }
    }

    val unfilteredRootNode = treeComponent.rootNode
    recurse(unfilteredRootNode)

    // Need to ensure the tree selection is now valid by selecting any children of nodes that are selected, or any nodes where all children are
    // selected.
    makeTreeValid(selectedItems.toList)
    treeComponent.repaint

    generateFromSelection
  }

  /**
   * This goes through a tree and given the selected items (supplied), ensures all parents / children are selected as required.
   */
  private def makeTreeValid(selItems:List[DefaultMutableTreeNode]) {
    // If the selected items have any nodes in them, de/select all their children.
    for (selectedItem <- selItems) {
      val currentSelected = selectedItem.getUserObject.asInstanceOf[CheckBoxListElement].selected
      recurse(selectedItem)
      def recurse(node:DefaultMutableTreeNode) {
        val element = node.getUserObject.asInstanceOf[CheckBoxListElement]
        if (element.value != OtherValue) {
          element.selected = currentSelected
        }
        val enum = node.children
        while (enum.hasMoreElements) {
          recurse(enum.nextElement.asInstanceOf[DefaultMutableTreeNode])
        }
      }
    }
    // If any of the selected items are deselected, ensure their parents are deselected as well.
    for (selectedItem <- selItems) {
      val initElement = selectedItem.getUserObject.asInstanceOf[CheckBoxListElement]
      val currentSelected = initElement.selected
      if (!currentSelected && (initElement.value != OtherValue)) {
        def deselectParent(node:DefaultMutableTreeNode) {
          val element = node.getUserObject.asInstanceOf[CheckBoxListElement]
          if (element.selected) {
            element.selected = false
          }
          val parent = node.getParent
          if (parent != null) {
            deselectParent(parent.asInstanceOf[DefaultMutableTreeNode])
          }
        }
        deselectParent(selectedItem)
      }
    }
    // If any of the selected items are selected, ensure that if all their parents children are selected, their parent is selected as well.
    for (selectedItem <- selItems) {
      val currentSelected = selectedItem.getUserObject.asInstanceOf[CheckBoxListElement].selected
      if (currentSelected) {
        def possiblySelectParent(node:DefaultMutableTreeNode) {
          val parent = node.getParent.asInstanceOf[DefaultMutableTreeNode]
          if (parent != null) {
            val enum = parent.children
            var allChildrenSelected = true
            while (enum.hasMoreElements) {
              val element = enum.nextElement.asInstanceOf[DefaultMutableTreeNode].getUserObject.asInstanceOf[CheckBoxListElement]
              if (!element.selected && (element.value != OtherValue)) {
                allChildrenSelected = false
              }
            }
            if (allChildrenSelected) {
              val element = parent.getUserObject.asInstanceOf[CheckBoxListElement]
              if (!element.selected) {
                element.selected = true
              }
              possiblySelectParent(parent)
            }
          }
        }
        possiblySelectParent(selectedItem)
      }
    }

    // OtherValue should be enabled if something but not everything is selected.
    val allElementsNoOther = treeComponent.getElements.filterNot(_.value == OtherValue)
    val allSelected = allElementsNoOther.forall(_.selected)
    val atLeastOneSelected = allElementsNoOther.exists(_.selected)
    filterHelper.setOtherValueEnabled(!allSelected && atLeastOneSelected)
  }

  private def selectedNodes:List[DefaultMutableTreeNode] = {
    val selectedNodes = new ListBuffer[DefaultMutableTreeNode]
    recurse(treeComponent.rootNode)
    def recurse(node:DefaultMutableTreeNode) {
      val cble = node.getUserObject.asInstanceOf[CheckBoxListElement]
      if (cble.selected) {
        selectedNodes += node
      } else {
        val enum = node.children
        while (enum.hasMoreElements) {
          recurse(enum.nextElement.asInstanceOf[DefaultMutableTreeNode])
        }
      }
    }
    selectedNodes.toList
  }

  private def generateFromSelection {
    // If the root node is selected, then everything is selected and return AllSelection.
    val selectionToUse = if (treeComponent.rootNode.getUserObject.asInstanceOf[CheckBoxListElement].selected) {
      AllSelection
    } else {
      // Get all of the selected elements out of the tree model.
      val selectedValues = selectedNodes.map(_.getUserObject.asInstanceOf[CheckBoxListElement].value)
      // This is a hack so the correct thing is selected. Required as some things don't have commodities at the moment. Can be removed
      // when this is fixed.
      val hackedSelectedValues = selectedValues.filterNot(_ == OtherValue).map(v => if (v == " ") "" else v)
      SomeSelection(Set() ++ hackedSelectedValues.toSet)
    }

    val eventToPublish = if (filterHelper.otherValueEnabled && filterHelper.otherValueSelected) {
      OtherValueSelectionChanged(this, selectionToUse)
    } else {
      FilterSelectionChanged(this, selectionToUse)
    }
    hidePopup
    publish(eventToPublish)
  }

  private def filterPopup(text:String) {
    val testText = text.trim.toLowerCase

    val rootNode = treeComponent.rootNode
    if (testText == "") {
      // Reset tree.
      treeComponent.peer.setModel(new DefaultTreeModel(rootNode))
      filterPanel.selectFilteredNodesButton.enabled = false
    } else {
      // Generate new tree with filter applied.
      def recurse(node:DefaultMutableTreeNode, newNode:DefaultMutableTreeNode) {
        if (!node.isLeaf) {
          if (node != rootNode) {
            val newParent = new DefaultMutableTreeNode(node.getUserObject)
            val enum = node.children
            while (enum.hasMoreElements) {
              val child = enum.nextElement.asInstanceOf[DefaultMutableTreeNode]
              recurse(child, newParent)
            }
            // Don't add the node if it doesn't have any children.
            if ((newParent.getChildCount != 0) ||
                    newParent.getUserObject.asInstanceOf[CheckBoxListElement].label.trim.toLowerCase.contains(testText)) {
              newNode.add(newParent)
            }
          } else {
            val enum = node.children
            while (enum.hasMoreElements) {
              val child = enum.nextElement.asInstanceOf[DefaultMutableTreeNode]
              recurse(child, newNode)
            }
          }
        } else {
          val userObject = node.getUserObject.asInstanceOf[CheckBoxListElement]
          if (userObject.label.trim.toLowerCase.contains(testText)) {
            newNode.add(new DefaultMutableTreeNode(userObject))
          }
        }
      }

      val filteredRootNode = new DefaultMutableTreeNode(rootNode.getUserObject)
      recurse(rootNode, filteredRootNode)
      treeComponent.peer.setModel(new DefaultTreeModel(filteredRootNode))
      filterPanel.selectFilteredNodesButton.enabled = true
    }
    // For the time being just expand everything.
    treeComponent.expandTree
  }
}

case class FilterHelper(tree:Tree, showOther:Boolean) {
  val valueToLabelMap = new HashMap[Any, String]
  private var otherValueElement:Option[CheckBoxListElement] = None

  def otherValueEnabled = otherValueElement match {
    case None => false
    case Some(ove) => ove.enabled
  }

  def otherValueSelected = otherValueElement match {
    case None => false
    case Some(ove) => ove.selected
  }

  def setOtherValueEnabled(enabled:Boolean) {
    otherValueElement match {
      case None =>
      case Some(cble) => {
        cble.enabled = enabled
      }
    }
  }

  def resetPopup(valuesAndSelection:(TreePivotFilter, Selection),
                 transforms:Option[FilterWithOtherTransform]):DefaultMutableTreeNode = {
    val (treePivotFilter, selection) = valuesAndSelection
    val rootNode = treePivotFilterToDefaultMutableTreeNode(treePivotFilter, selection, transforms)
    tree.model = new DefaultTreeModel(rootNode)
    tree.rootNode = rootNode
    tree.expandTree
    rootNode
  }

  private def treePivotFilterToDefaultMutableTreeNode(treePivotFilter:TreePivotFilter, selection:Selection,
                                                      transforms:Option[FilterWithOtherTransform]) = {
    def getDefaultMutableTreeNodeFromTreePivotFilterNode(treePivotFilterNode:TreePivotFilterNode) = {
      val value = treePivotFilterNode.value
      val label = treePivotFilterNode.label
      valueToLabelMap(value) = label

      val selected = transforms match {
        case None => selection match {
          case AllSelection => true
          case SomeSelection(selectedValues) if selectedValues.isEmpty => false
          case SomeSelection(selectedValues) => selectedValues.contains(value)
        }
        case Some(FilterWithOtherTransform(selectedValues)) => selectedValues.contains(value)
      }
      new DefaultMutableTreeNode(new CheckBoxListElement(selected, value, label, true), true)
    }
    def recurse(node:TreePivotFilterNode, parent:DefaultMutableTreeNode):DefaultMutableTreeNode = {
      val childNode = getDefaultMutableTreeNodeFromTreePivotFilterNode(node)
      if (node.children.nonEmpty) {
        for (child <- node.children) {
          recurse(child, childNode)
        }
      }
      if (parent != null) {
        parent.add(childNode)
      }
      childNode
    }
    val root = treePivotFilter.root
    val nodeToReturn = recurse(root, null)
    // Go through this and if any parents are selected, ensure that all their children are as well.
    def recurse2(node:DefaultMutableTreeNode) {
      val element = node.getUserObject.asInstanceOf[CheckBoxListElement]
      val selected = element.selected
      if (selected) {
        val enum = node.children
        while (enum.hasMoreElements) {
          val child = enum.nextElement.asInstanceOf[DefaultMutableTreeNode]
          val childCheck = child.getUserObject.asInstanceOf[CheckBoxListElement]
          if (!childCheck.selected) {
            childCheck.selected = true
          }
        }
      }
      val enum = node.children
      while (enum.hasMoreElements) {
        recurse2(enum.nextElement.asInstanceOf[DefaultMutableTreeNode])
      }
    }
    recurse2(nodeToReturn)

    if (showOther) {
      val (selected, enabled) = transforms match {
        case None => {
          val en = selection match {
            case AllSelection => false
            case SomeSelection(selectedValues) if selectedValues.isEmpty => false
            case SomeSelection(selectedValues) => true
            case _ => false
          }
          (false,en)
        }
        case Some(FilterWithOtherTransform(values)) if values.nonEmpty => (true,true)
        case _ => (false,false)
      }
      otherValueElement = Some(new CheckBoxListElement(selected, OtherValue, FilterWithOtherTransform.OtherValue.toString, enabled))
      nodeToReturn.insert(new DefaultMutableTreeNode(otherValueElement.get, false), 0)
    }

    nodeToReturn
  }
}

case object OtherValue

class TreePanelFilterPanel extends MigPanel("insets 0", "[p]0[p]0[p]2lp[p]") {
  val search = new FixedImagePanel(StarlingIcons.im("/icons/16x16find.png"))
  val searchHolder = new MigPanel("insets 0") {
    border = MatteBorder(1, 1, 1, 0, BorderColour)
    add(search, "gapbefore 2lp, gapafter 2lp, align center center, push")
  }
  val textField = new TextField {
    override protected def paintBorder(g:Graphics2D) = {
      super.paintBorder(g)
      val width = size.width - 1
      val height = size.height - 2
      g.setColor(Color.WHITE)
      g.drawLine(width, 1, width, height)
      g.setColor(BorderColour.brighter)
      g.drawLine(0, 1, 0, height)
    }
  }
  val clearImage = new FixedImagePanel(StarlingIcons.im("/icons/closeHovered.png")) {
    cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
  }
  val clearImageHolder = new MigPanel("insets 0") {
    border = MatteBorder(1, 0, 1, 1, BorderColour)
    background = Color.WHITE
    add(clearImage, "align center center, push")
  }
  val selectFilteredNodesButton = new Button {
    text = "Select Filtered"
    mnemonic = Key.S
    tooltip = "Selects the nodes or leafs that match the filter text"
    icon = StarlingIcons.icon("/icons/16x16_select_filter.png")
    enabled = false
    reactions += {case ButtonClicked(e) => TreePanelFilterPanel.this.publish(SelectFiltered(textField.text))}
  }
  textField.minimumSize = selectFilteredNodesButton.preferredSize
  reactions += {
    case KeyPressed(`textField`, Key.Escape, _, _) => textField.text = ""
    case KeyReleased(`textField`, _, _, _) => {
      // Ensure the fields aren't being displayed here.
      publish(FilterPopupEvent(textField.text))
    }
    case MouseClicked(`clearImage`, _, _, _, _) => {
      textField.text = ""
      publish(FilterPopupEvent(""))
    }
    case KeyPressed(`textField`, Key.Down, _, _) => publish(DownPressedFromFilterAreaEvent)
  }
  listenTo(textField.keys, clearImage.mouse.clicks)
  add(searchHolder, "grow")
  add(textField, "push, grow")
  add(clearImageHolder, "grow")
  add(selectFilteredNodesButton, "grow")
}

case object DownPressedFromFilterAreaEvent extends Event
case class FilterPopupEvent(text:String) extends Event
case object GenerateFromSelectionEvent extends Event
case class CancelEvent(source:Component) extends Event
case class SelectFiltered(text:String) extends Event
case class FilterSelectionChanged(source:Component, selection:Selection) extends Event
case class OtherValueSelectionChanged(source:Component, selection:Selection) extends Event

class TreePanelButtonPanel extends MigPanel("insets 0, al right", "[p]2lp[p]") {
  val okButton = new Button {
    text = "OK"
    reactions += {case ButtonClicked(b) => TreePanelButtonPanel.this.publish(GenerateFromSelectionEvent)}
  }
  val cancelButton = new Button {
    text = "Cancel"
    reactions += {case ButtonClicked(b) => TreePanelButtonPanel.this.publish(CancelEvent(TreePanelButtonPanel.this))}
  }
  add(okButton, "sg")
  add(cancelButton, "sg")
}

class Tree extends Component {
  var rootNode:DefaultMutableTreeNode = null
  // TODO [08 Dec 2010] get rid of the magic number and put in the proper number.
  val magicNumberForTreeSize = 20
  lazy val treeCellRenderer = new CheckBoxElementTreeCellRenderer

  override lazy val peer = new JTree {
    setToggleClickCount(0)
    setCellRenderer(treeCellRenderer)
  }

  def model = peer.getModel
  def model_=(m:TreeModel) = peer.setModel(m)

  def expandTree {
    var row = 0
    while (row < peer.getRowCount) {
      peer.expandRow(row)
      row += 1
    }
  }

  def getElements = {
    val l = new ListBuffer[CheckBoxListElement]
    def recurse(node:DefaultMutableTreeNode) {
      l += node.getUserObject.asInstanceOf[CheckBoxListElement]
      val enum = node.children
      while (enum.hasMoreElements) {
        recurse(enum.nextElement.asInstanceOf[DefaultMutableTreeNode])
      }
    }
    recurse(rootNode)
    l.toList
  }

  def getOtherValueElement = {
    getElements.find(_.value == OtherValue)
  }
}

class CheckBoxElementTreeCellRenderer extends TreeCellRenderer {
  var mouseOverRow = -1
  private val ren = new MigPanel("insets 0") {
    val checkBox = new CheckBox {
      opaque = false
      text = ""
      border = EmptyBorder(2, 2, 2, 0)
    }
    val label = new Label {
      text = " "
    }
    private val defaultTextColour = label.foreground
    def resetLabelTextColour = label.foreground = defaultTextColour
    add(checkBox)
    add(label)
    override def background_=(c:Color) = {
      super.background = c
      checkBox.background = c
    }

    override def enabled_=(b:Boolean) = {
      super.enabled = b
      checkBox.enabled = b
      label.enabled = b
    }
  }

  def getTreeCellRendererComponent(tree:JTree, value:Any, selected:Boolean, expanded:Boolean, leaf:Boolean, row:Int, hasFocus:Boolean) = {
    val elem = value.asInstanceOf[DefaultMutableTreeNode].getUserObject.asInstanceOf[CheckBoxListElement]
    ren.label.text = elem.label.trim + " "
    ren.checkBox.selected = elem.selected
    ren.enabled = elem.enabled
    val otherValue = elem.value == OtherValue

    def setBackground {
      ren.background = if (selected) {
        UIManager.getColor("List.selectionBackground")
      } else if (row == mouseOverRow) {
        MouseOverColour
      } else if (otherValue && elem.enabled) {
        GuiUtils.OtherValueTotalColour
      }
      else {
        Color.WHITE
      }
    }

    if (elem.enabled) {
      setBackground
      if (otherValue) ren.label.foreground = Color.BLUE else ren.resetLabelTextColour
    } else {
      ren.background = Color.WHITE
    }

    if (otherValue) {
      ren.tooltip = "Group unselected values and display"
    } else {
      ren.tooltip = null
    }

    ren.revalidate
    ren.peer
  }
}