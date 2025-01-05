import { getStore } from './memoize.js';
import { get2dContext, getGlobalScale } from './canvas-tools.js';
import { calculateDistance } from './map-measurements.js';

const getFixSizes = (scale) => {
  scale = scale ?? getGlobalScale();

  const radius = 10 * scale;
  const dotRadius = 3 * scale;
  const margin = 3 * scale;

  return { radius, dotRadius, margin };
};

/**
 * Draw a 'fix' element.
 * @param x The x position within the canvas where to place the center of the 'fix'.
 * @param y The y position within the canvas where to place the center of the 'fix'.
 * @param scale Scale to use for this call only, typically used when drawing the 'fix' on a button.
 * If the scale is not provided, the value returned by getGlobalScale() is used.
 * @param context The context to draw on, uses the main canvas context by default.  Typically used
 * when drawing on a button.
 */
export const drawFix = ({ position, scale, context }) => {
  const { radius, dotRadius, margin } = getFixSizes(scale);

  const c = context ?? get2dContext();
  const { x, y } = position;

  // draw containing area to ensure visibility on all backgrounds
  c.beginPath();
  c.arc(x, y, radius + margin, 0, Math.PI * 2);
  c.fillStyle = 'white';
  c.fill();
  c.closePath();

  // draw outer circle
  c.beginPath();
  c.arc(x, y, radius, 0, Math.PI * 2);
  c.lineWidth = 2;
  c.strokeStyle = 'black';
  c.stroke();
  c.closePath();

  // draw inner dot
  c.beginPath();
  c.arc(x, y, dotRadius, 0, Math.PI * 2);
  c.fillStyle = 'black';
  c.fill();
  c.closePath();
};

export const isInFixElementArea = ({ elementPosition, currentPosition }) => {
  const { radius, margin } = getFixSizes();
  const distance = calculateDistance({ anchor: currentPosition, point: elementPosition });
  return distance <= radius + margin;
};

