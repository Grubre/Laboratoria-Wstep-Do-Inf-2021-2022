import javafx.event.EventHandler;
import javafx.scene.Node;
import javafx.scene.SubScene;
import javafx.scene.input.MouseEvent;
/**
 * Class that adds the dragging and rotation properties to an object.
 */
public class PropertyAdder {
    private final Node target;
    private double anchorRotate;
    private double initialX;
    private double initialY;
    private double rotationX;
    private EventHandler<MouseEvent> setAnchor;
    private EventHandler<MouseEvent> updatePositionOnDrag;
    private int cycleStatus = 0;
    /**
     * Constructor that takes the target, creates the handlers and adds event filters
     */
    public PropertyAdder(Node target) {
        this.target = target;
        createHandlers();
        target.addEventFilter(MouseEvent.MOUSE_PRESSED, setAnchor);
        target.addEventFilter(MouseEvent.MOUSE_DRAGGED, updatePositionOnDrag);
    }
    /**
     * Method that creates event handlers for mouse dragging and rotating
     */
    private void createHandlers() {
        setAnchor = event -> {
            if(MainScene.getState() != MainScene.State.MODIFY)
                return;
            if (event.isPrimaryButtonDown()) {
                cycleStatus = 1;
                initialX = event.getSceneX();
                initialY = event.getSceneY();
            }
            if (event.isMiddleButtonDown()) {
                cycleStatus = 0;
                anchorRotate = target.getRotate();

            }
            rotationX = event.getSceneX();
            if(event.isSecondaryButtonDown())
                cycleStatus = 0;
        };
        updatePositionOnDrag = event -> {
            if(MainScene.getState() != MainScene.State.MODIFY)
                return;
            if (cycleStatus != 0) {
                target.setTranslateX(target.getTranslateX() + event.getSceneX() - initialX);
                target.setTranslateY(target.getTranslateY() + event.getSceneY() - initialY);
                initialX = event.getSceneX();
                initialY = event.getSceneY();
            }
            if(event.isMiddleButtonDown())
            {
                target.setRotate(event.getSceneX() - rotationX + anchorRotate);
            }

        };
    }

}
