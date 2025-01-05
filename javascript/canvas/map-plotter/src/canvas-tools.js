import { getStore } from './memoize.js';
import { memoize } from './memoize.js';

export const getCanvas = memoize(() => document.getElementById('map-table'));
export const get2dContext = memoize(() => getCanvas().getContext('2d'));

export const getCanvasRelativeMousePos = ({ canvas, evt }) => {
  let rect = canvas.getBoundingClientRect();
  return {
    x: evt.clientX - rect.left,
    y: evt.clientY - rect.top
  };
}

export const keepInCanvasBounds = ({ canvas, position }) => {
  let isInCanvas = true;

  if (position.x < 0) {
    position.x = 0;
    isInCanvas = false;
  }
  if (position.x > canvas.width) {
    position.x = canvas.width;
    isInCanvas = false;
  }

  if (position.y < 0) {
    position.y = 0;
    isInCanvas = false;
  }
  if (position.y > canvas.height) {
    position.y = canvas.height;
    isInCanvas = false;
  }
  return { position, isInCanvas };
};

export const clearCanvas = () => {
  const c = get2dContext();
  const canvas = getCanvas();
  c.globalAlpha = 1.0;
  c.fillStyle = 'wheat';
  c.fillRect(0, 0, canvas.width, canvas.height);

  // fill area with different color than background to test elements visibility
  c.globalAlpha = 1.0;
  c.fillStyle = 'black';
  c.fillRect(20, 20, 80, 80);

};

export const getGlobalScale = () => getStore()['scale'] ?? 1;

