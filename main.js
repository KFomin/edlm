import Main from './src/Main.elm'
import dialogPolyfill from 'dialog-polyfill'

let app = Main.init({
    node: document.getElementById('app'),
})
//
// app.ports.toggleGroupDialog.subscribe(id => {
//     const dialog = document.querySelector(`#${id}`)
//     dialogPolyfill.registerDialog(dialog);
//
//     const handleBackdropClick = (event) => {
//         let dialogRect = dialog.getBoundingClientRect();
//         if (
//             event.clientX > dialogRect.right ||
//             event.clientX < dialogRect.left ||
//             event.clientY < dialogRect.top ||
//             event.clientY > dialogRect.bottom
//         ) {
//             dialog.close();
//         }
//     };
//
//     if (dialog.open) {
//         dialog.close();
//         dialog.removeEventListener('click', handleBackdropClick);
//     } else {
//         dialog.showModal();
//         dialog.addEventListener('click', handleBackdropClick);
//     }
//
// });
