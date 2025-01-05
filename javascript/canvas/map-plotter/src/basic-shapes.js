import { getStore } from './memoize.js';
import { get2dContext } from './canvas-tools.js';

/**
 * Draw a line for the current element, starting at `element.startPosition`.
 * If `element.isBeingBuilt` is true, it draws to the current cursor position.
 * Else, it draws to `element.position`.
 */
export const drawLine = (element) => {
  if (!element.startPosition) return;

  const { x: startX, y: startY } = element.startPosition;
  element.position = element.isBeingBuilt ? getStore()['cursor'] : element.position;
  const { x: endX, y: endY } = element.position;

  const c = get2dContext();

  // draw containing line for contrast on any background
  c.beginPath();
  c.moveTo(startX, startY);
  c.lineTo(endX, endY);
  c.lineWidth = 4;
  c.strokeStyle = 'white';
  c.stroke();
  c.closePath();

  // draw the actual line
  c.beginPath();
  c.moveTo(startX, startY);
  c.lineTo(endX, endY);
  c.lineWidth = 2;
  c.strokeStyle = 'black';
  c.stroke();
  c.closePath();
};

