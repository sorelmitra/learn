import { getStore } from './memoize.js';
import {
  getCanvas,
  getCanvasRelativeMousePos,
  keepInCanvasBounds,
} from './canvas-tools.js';

const mouseMoveHandler = (e) => {
  const canvas = getCanvas();
  const cursor = getCanvasRelativeMousePos({ canvas, evt: e });
  const keepInCanvasResponse = keepInCanvasBounds({ canvas, position: cursor });
  getStore()['isCursorInCanvas'] = keepInCanvasResponse.isInCanvas;
  if (getStore()['isCursorInCanvas']) {
    getStore()['cursor'] = keepInCanvasResponse.position;
  }
};

export const handleMouseMove = ({ handle }) => {
  if (handle) {
    addEventListener('mousemove', mouseMoveHandler);
  } else {
    removeEventListener('mousemove', mouseMoveHandler);
  }
};

const mouseUpHandler = () => {
  const handler = getStore()['cursorUpHandler'];
  if (!getStore()['isCursorInCanvas']) return;
  if (getStore()['forbidPlacement']) return;
  handler();
};

export const handleMouseUp = ({ handler }) => {
  if (handler) {
    getStore()['cursorUpHandler'] = handler;
    addEventListener('mouseup', mouseUpHandler);
  } else {
    getStore()['cursorUpHandler'] = null;
    removeEventListener('mouseup', mouseUpHandler);
  }
};

export const setOnTargetCursor = ({ cursorStyle, onTargetCursorStyle }) => {
  if (!onTargetCursorStyle) return;

  const canvas = getCanvas();
  if (getStore()['mapElementPointer']) {
    canvas.style.cursor = onTargetCursorStyle;
  } else {
    canvas.style.cursor = cursorStyle;
  }
};

