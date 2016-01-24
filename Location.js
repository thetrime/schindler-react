var React = require('react');
var AppDispatcher = require('./AppDispatcher');

module.exports = React.createClass(
    {
        render: function()
        {
            return (<tr>
                    <th colSpan="2">
                    {this.props.location}
                    </th>
                    </tr>);
                    
        }
    });