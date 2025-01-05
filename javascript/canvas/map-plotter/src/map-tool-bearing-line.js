import { getStore } from './memoize.js';
import { get2dContext } from './canvas-tools.js';
import { calculateDistance, calculateBearing } from './map-measurements.js';
import { drawLine } from './basic-shapes.js';

/**
 * Draw a 'bearing line' element, which consists of a line and an arrow.
 */
export const drawBearingLine = (element) => {
  if (!element.startPosition) return;

  drawLine(element);

  const { x, y } = element.position;
};

export const isInBearingLineElementArea = ({ elementPosition, currentPosition }) => {
  const distance = calculateDistance({ anchor: currentPosition, point: elementPosition });
  const bearing = calculateBearing({ anchor: elementPosition, point: currentPosition });
};

