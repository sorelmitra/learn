import { getStore } from './memoize.js';
import { getCanvas} from './canvas-tools.js';
import {
  handleMouseMove,
  handleMouseUp,
  setOnTargetCursor,
} from './mouse-handling.js';
import {
  getElementAdder,
  storeCurrentMapElement,
  deleteCurrentMapElement,
  setForbidPlacement,
} from './map-editing.js';
import { drawFix, isInFixElementArea } from './map-tool-fix.js';
import { drawDeadReckon, isInDeadReckonElementArea } from './map-tool-dead-reckon.js';
import { drawBearingLine, isInBearingLineElementArea } from './map-tool-bearing-line.js';

const unselectAllOtherTools = (toolName) => {
  Object.keys(getStore()['toolButtonStates']).map(s => {
    if (s === toolName) return;
    getStore()['toolButtonStates'][s] = true;
    getStore()['toolButtons'][s]['onToolChosen']();
  });
};

const getTargetProcessor = (toolConfig) => () => {
  if (!toolConfig.onTargetHandlers?.length) return;

  for (const handler of toolConfig.onTargetHandlers) {
    handler(toolConfig);
  }
};

const getOnToolChosenHandler = (toolName) => () => {
  const toolConfig = getStore()['toolButtons'][toolName];
  const { staticButtonId, onMouseUp, onCursor, cursorStyle } = toolConfig;
  const isButtonPressed = getStore()['toolButtonStates'][toolName] ?? false;

  const canvas = getCanvas();
  const buttonElement = document.getElementById(`map-tool-canvas-${toolName}`) ?? document.getElementById(staticButtonId);

  if (isButtonPressed) {
    handleMouseMove({ handle: false });
    canvas.style.cursor = 'default';
    clearInterval(toolConfig.onTargetCursorIntervalId);
    handleMouseUp({ handler: null });
    getStore()['onCursor'] = null;
    buttonElement.classList.remove('map-tool-button-canvas-inverse');
  } else {
    unselectAllOtherTools(toolName);
    handleMouseMove({ handle: true });
    canvas.style.cursor = cursorStyle;
    toolConfig.onTargetCursorIntervalId = setInterval(getTargetProcessor(toolConfig), 100);
    handleMouseUp({ handler: onMouseUp });
    getStore()['onCursor'] = onCursor;
    buttonElement.classList.add('map-tool-button-canvas-inverse');
  }

  getStore()['toolButtonStates'][toolName] = !isButtonPressed;
};

const makeCanvas = (toolName) => {
  const canvas = document.createElement('canvas');
  canvas.classList.add('map-tool-button-canvas');
  canvas.setAttribute('width', 32);
  canvas.setAttribute('height', 32);
  canvas.setAttribute('id', `map-tool-canvas-${toolName}`);
  canvas.style.cursor = 'pointer';
  return canvas;
};

const addButton = (toolName) => {
  const button = document.createElement('button');
  button.classList.add('map-tool-button');

  const canvas = makeCanvas(toolName);
  button.appendChild(canvas);

  const container = document.getElementById('map-tools-container');
  container.appendChild(button);

  return canvas;
};

/**
 * Adds all dynamic tool buttons, defined in `getStore()['toolButtons']`, to the toolbar.
 * 'Dynamic' tool buttons are the one that have a drawn image as opposed to a statically loaded one.
 * They do NOT have the `staticButtonId` and `staticButtonImageUrl` properties.
 */
const addDynamicToolButtons = () => {
  for (const toolName of Object.keys(getStore()['toolButtons'])) {
    const { staticButtonId, drawButtonFunc } = getStore()['toolButtons'][toolName];
    if (staticButtonId) continue;

    const canvas = addButton(toolName);
    canvas.onclick = getOnToolChosenHandler(toolName);
    getStore()['toolButtons'][toolName]['onToolChosen'] = canvas.onclick;

    const x = canvas.width / 2.5;
    const y = canvas.height / 2.5;
    const context = canvas.getContext('2d');
    drawButtonFunc({ position: { x, y }, scale: 0.75, context });
  }
};

/**
 * Adds all static tool buttons, defined in `getStore()['toolButtons']`, to the toolbar.
 * 'Static' tool buttons are the one that have a static, loaded, image.
 * They have `staticButtonId` and `staticButtonImageUrl` properties.
 */
const addStaticToolButtons = () => {
  const buttonIds = [];

  for (const toolName of Object.keys(getStore()['toolButtons'])) {
    const { staticButtonId, staticButtonImageUrl } = getStore()['toolButtons'][toolName];
    if (!staticButtonId) continue;

    const canvas = addButton(toolName);
    const image = new Image();
    image.src = staticButtonImageUrl;
    image.onload = () => {
      canvas.getContext('2d').drawImage(image, 4, 4);
    };

    canvas.onclick = getOnToolChosenHandler(toolName);
    getStore()['toolButtons'][toolName]['onToolChosen'] = canvas.onclick;
  }
};

/**
 * Initializes the toolbox.
 *
 * The buttons are defined in the store, inside the `toolButtons` object:
 *
 * Each button is defined by a property of that object, that contains a sub-object
 * with all the properties for this button:
 *
 * - staticButtonId: For static buttons only, the ID to put on the button. 
 *   For dynamic buttons it is null, as the ID is automatically generated.
 *
 * - staticButtonImageUrl: For static buttons only, the URL of the image
 *   to display. Null for dynamic buttons, as those are drawn from the app.
 *
 * - drawButtonFunc: The function used to draw the button.  Null for static buttons.
 *
 * - cursorStyle: Mouse cursor to use when the button is pressed, i.e. in use.
 *
 * - onTargetCursorStyle: Mouse cursor to use when the pointer is over a target.
 *   A target is any map element, i.e. a fix, a line.
 *
 * - onMouseUp: Function to call when the mouse is pressed when using the tool.
 *
 * - onCursor: Function that draws an overlay image at the mouse position.
 *   The image is drawn in a transparent manner.
 *   Use null if you do not want an overlay image to be drawn at the mouse position.
 *
 * - onToolChosen: Function that defines what happens when the tool
 *   button is actually pressed.  Added automatically by the add*ToolButtons functions.
 */
export const initMapToolbox = () => {
  getStore()['toolButtonStates'] = {};

  const pencilCursor = 'url("art/cursor-pencil.png") 3 22, default';
  const pencilNotAllowedCursor = 'url("art/cursor-pencil-not-allowed.png") 3 22, not-allowed';

  getStore()['toolButtons'] = {
    fix: {
      drawButtonFunc: drawFix,
      cursorStyle: pencilCursor,
      onTargetCursorStyle: pencilNotAllowedCursor,
      onMouseUp: getElementAdder({ draw: drawFix, isInArea: isInFixElementArea }),
      onCursor: { drawFunc: drawFix },
      onTargetHandlers: [storeCurrentMapElement, setOnTargetCursor, setForbidPlacement],
    },
    deadReckonPosition: {
      staticButtonId: 'map-tool-button-dr-pos',
      staticButtonImageUrl: './art/button-dead-reckon.png',
      cursorStyle: pencilNotAllowedCursor,
      onTargetCursorStyle: pencilCursor,
      onMouseUp: getElementAdder({ draw: drawDeadReckon, isInArea: isInDeadReckonElementArea, isMultiPath: true, anchorToExistingElement: true }),
      onTargetHandlers: [storeCurrentMapElement, setOnTargetCursor],
    },
    bearingLine: {
      staticButtonId: 'map-tool-button-bearing-line',
      staticButtonImageUrl: './art/button-line.png',
      cursorStyle: pencilCursor,
      onTargetCursorStyle: pencilNotAllowedCursor,
      onMouseUp: getElementAdder({ draw: drawBearingLine, isInArea: isInBearingLineElementArea, isMultiPath: true }),
      onTargetHandlers: [storeCurrentMapElement, setOnTargetCursor],
    },
    erase: {
      staticButtonId: 'map-tool-button-delete',
      staticButtonImageUrl: './art/button-eraser.png',
      cursorStyle: 'url("art/cursor-cross.png") 12 12, crosshair',
      onTargetCursorStyle: 'url("art/cursor-eraser.png") 7 21, no-drop',
      onMouseUp: deleteCurrentMapElement,
      onTargetHandlers: [storeCurrentMapElement, setOnTargetCursor],
    },
  };

  addDynamicToolButtons();
  addStaticToolButtons();
};

