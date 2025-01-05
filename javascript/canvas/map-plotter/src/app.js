import { getStore } from './memoize.js';
import {
  getCanvas,
  clearCanvas,
} from './canvas-tools.js';
import { showCursorOverlay, plot } from './map-editing.js';
import { initMapToolbox } from './map-toolbox.js';

const animate = () => {
  requestAnimationFrame(animate);

  clearCanvas();
  plot();

  showCursorOverlay();
};

const initMapTable = () => {
  const canvas = getCanvas();
  getStore()['cursor'] = { x: canvas.width / 2, y: canvas.height / 2 };
};

const initMapElements = () => {
  getStore()['elements'] = [];
};

const main = () => {
  initMapToolbox();
  initMapTable();
  initMapElements();

  animate();
}

main();

